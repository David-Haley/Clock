#!/usr/bin/env python3
"""
WebSocket <-> UDP bridge for the IOT Clock simulator.

Receives Ada binary Status_Records from the clock's UDP server (port 50004),
converts them to JSON, and broadcasts to browsers over WebSocket (port 8765).
Forwards browser JSON commands back as Request_Records binary to port 50003.

Usage:
    uv run bridge.py [clock-host]   # default: 127.0.0.1
    BRIDGE_HTTP_PORT=8000 uv run --with websockets web/bridge.py [clock-host]
                                        # also serves web/ on HTTP port 8000

Environment variables:
    BRIDGE_WS_HOST   WebSocket bind address (default: 0.0.0.0)
    BRIDGE_HTTP_PORT HTTP port for serving web/ files; 0 = disabled (default: 0)

─── Ada wire layout (aarch64 little-endian, GNAT defaults) ──────────────────

Status_Records (284 bytes):
  off  0   8s    Clock_Version          String(1..8)
  off  8   B     Request                Requests enum (uint8 — 7 values fit in 1 byte)
  off  9   ?     Diagnostic_Toggle      Boolean (1 byte)
  off 10   8s    User_Interface_Version String(1..8)
  off 18   6x    padding                (align int64 Current_Time to offset 24)
  off 24   q     Current_Time           Ada.Calendar.Time (int64 ns since epoch)
  off 32   ?     Chime_Enabled          Boolean
  off 33   ?     Chime_Toggle           Boolean
  off 34   2x    padding
  off 36   i     Chime_Volume           Natural 0..100 (int32)
  off 40   H     Ambient_Light          Unsigned_16
  off 42   H     AL_Test_Value          Unsigned_16
  off 44   80s   Current_Item           String(1..80)
  off 124  160H  LED_Array              array(10 drivers × 16 channels) of Greyscales (uint16)
Total: 444 bytes  (Status_Records'Size = 3552 bits, no trailing padding)

Layout verified empirically: ui_version decoded as '50510\\x00\\x00\\x00' with old
'I' (4-byte) format confirmed Request is 1 byte (GNAT uses smallest integer type).

LED_Array row order (LED_Drivers enum):
  0 Sweep_00_14  1 Sweep_15_29  2 Sweep_30_44  3 Sweep_45_59
  4 Seconds_Drv  5 Minutes_Drv  6 Hours_Drv
  7 Years_Drv    8 Months_Drv   9 Days_Drv
Each row: channels 0-7 = tens digit (segs a,b,c,d,e,f,g,DP)
          channels 8-15 = units digit

Request_Records (16 bytes):
  off  0   8s    User_Interface_Version
  off  8   I     Request                Requests enum (uint32)
  off 12   ?     Diagnostic_Toggle      Boolean
  off 13   1x    padding
  off 14   H     Ambient_Override       Greyscales (uint16); 0 = use sensor
Total: 16 bytes  (Request_Records'Size = 128 bits)

Ada.Calendar.Time on GNAT/Linux = nanoseconds since Jan 1 1970 (POSIX epoch).
If the value looks unreasonable the bridge falls back to datetime.now().

Layout verification (add to a test Ada program):
  Put_Line(Status_Records'Size'Image);   -- expect 3552
  Put_Line(Request_Records'Size'Image);  -- expect 128
"""

import asyncio
import json
import logging
import os
import socket
import struct
import sys
import threading
from datetime import datetime, timezone
from http.server import SimpleHTTPRequestHandler, HTTPServer
from pathlib import Path

import websockets
from websockets.server import serve

# ── Configuration ─────────────────────────────────────────────────────────────
CLOCK_HOST    = sys.argv[1] if len(sys.argv) > 1 else "127.0.0.1"
REQUEST_PORT  = 50003
RESPONSE_PORT = 50004
WS_HOST       = os.environ.get("BRIDGE_WS_HOST", "0.0.0.0")
WS_PORT       = 8765
HTTP_PORT     = int(os.environ.get("BRIDGE_HTTP_PORT", "0"))

INTERFACE_VERSION = b"20250510"

# ── Ada enum ──────────────────────────────────────────────────────────────────
# type Requests is (Toggle_Chime, Cycle_Sweep, Volume_Up, Volume_Down,
#                   Volume_Test, Get_Status, Exit_User_Interface);
REQUEST_ENUM = {
    "Toggle_Chime":        0,
    "Cycle_Sweep":         1,
    "Volume_Up":           2,
    "Volume_Down":         3,
    "Volume_Test":         4,
    "Get_Status":          5,
    "Exit_User_Interface": 6,
}

# ── Struct formats ────────────────────────────────────────────────────────────
STATUS_FMT   = "<8sB?8s6xq??2xiHH80s160H"
REQUEST_FMT  = "<8sI?xH"

STATUS_SIZE  = struct.calcsize(STATUS_FMT)   # must be 444
REQUEST_SIZE = struct.calcsize(REQUEST_FMT)  # must be 16

assert STATUS_SIZE  == 444, f"STATUS_FMT calcsize={STATUS_SIZE}, expected 444"
assert REQUEST_SIZE == 16,  f"REQUEST_FMT calcsize={REQUEST_SIZE}, expected 16"

_STATUS_STRUCT  = struct.Struct(STATUS_FMT)
_REQUEST_STRUCT = struct.Struct(REQUEST_FMT)

# ── Optional HTTP static file server ──────────────────────────────────────────
class _IndexOnlyHandler(SimpleHTTPRequestHandler):
    """Allows only /index.html; redirects / to it; 404 everything else."""

    def do_GET(self):
        if self.path == "/":
            self.send_response(302)
            self.send_header("Location", "/index.html")
            self.end_headers()
        elif self.path == "/index.html":
            super().do_GET()
        else:
            self.send_error(404)

    def log_message(self, format, *args):
        logging.debug("HTTP %s", format % args)

def _start_http_server(port: int) -> None:
    """Serve only index.html over HTTP for LAN access; redirect / to /index.html."""
    web_dir = str(Path(__file__).parent)
    handler = lambda *a, **kw: _IndexOnlyHandler(*a, directory=web_dir, **kw)
    server = HTTPServer(("", port), handler)
    logging.info("HTTP file server: http://0.0.0.0:%d/index.html", port)
    server.serve_forever()

# ── Ada Calendar.Time decoding ────────────────────────────────────────────────
def decode_ada_time(ns: int) -> str:
    """Convert Ada Calendar.Time nanoseconds to local HH:MM:SS string."""
    try:
        sec = ns / 1_000_000_000
        if not (0 < sec < 4_000_000_000):   # sanity: 1970-2096
            raise ValueError(f"Time out of range: {sec}")
        return datetime.fromtimestamp(sec, tz=timezone.utc).astimezone().strftime("%H:%M:%S")
    except Exception:
        return datetime.now().strftime("%H:%M:%S")

# ── Status decode ─────────────────────────────────────────────────────────────
def decode_status(data: bytes) -> dict:
    """Unpack a Status_Records binary blob → JSON-serialisable dict."""
    if len(data) < STATUS_SIZE:
        raise ValueError(f"Packet too short: {len(data)} < {STATUS_SIZE}")

    unpacked = _STATUS_STRUCT.unpack_from(data)
    # Tuple indices after unpack (padding bytes are not present as values):
    # 0  clock_version  (bytes)
    # 1  request_enum   (int)
    # 2  diag_toggle    (bool)
    # 3  ui_version     (bytes)
    # 4  current_time_ns (int64)
    # 5  chime_enabled  (bool)
    # 6  chime_toggle   (bool)
    # 7  chime_volume   (int32)
    # 8  ambient_light  (uint16)
    # 9  al_test_value  (uint16)
    # 10 current_item   (bytes)
    # 11..170  160 LED greyscale values (uint16, 0-4095)

    led_vals = unpacked[11:]   # 160 elements
    leds = [
        [led_vals[r * 16 + c] for c in range(16)]
        for r in range(10)
    ]

    return {
        "clock_version": unpacked[0].decode("ascii", errors="replace").strip(),
        "ui_version":    unpacked[3].decode("ascii", errors="replace").strip(),
        "time":          decode_ada_time(unpacked[4]),
        "chime_enabled": unpacked[5],
        "chime_toggle":  unpacked[6],
        "volume":        unpacked[7],
        "ambient_light": unpacked[8],
        "al_test_value": unpacked[9],
        "current_item":  unpacked[10].decode("ascii", errors="replace").rstrip(),
        "leds":          leds,
    }

# ── Request encode ────────────────────────────────────────────────────────────
def encode_request(request_name: str, diag_toggle: bool = False,
                   ambient_override: int = 0) -> bytes:
    enum_val = REQUEST_ENUM.get(request_name, REQUEST_ENUM["Get_Status"])
    return _REQUEST_STRUCT.pack(INTERFACE_VERSION, enum_val, diag_toggle,
                                ambient_override)

# ── WebSocket server ──────────────────────────────────────────────────────────
connected_clients: set = set()
_udp_tx: socket.socket | None = None
_ambient_override: int = 0   # 0 = use sensor; >0 = simulator override


async def ws_handler(websocket):
    global _ambient_override
    connected_clients.add(websocket)
    logging.info("Browser connected  (total: %d)", len(connected_clients))
    try:
        async for raw in websocket:
            try:
                cmd = json.loads(raw)
                if "ambient_override" in cmd:
                    _ambient_override = int(cmd["ambient_override"]) & 0xFFFF
                pkt = encode_request(cmd.get("request", "Get_Status"),
                                     ambient_override=_ambient_override)
                _udp_tx.sendto(pkt, (CLOCK_HOST, REQUEST_PORT))
            except Exception as exc:
                logging.warning("Bad browser command: %s", exc)
    except websockets.exceptions.ConnectionClosed:
        pass
    finally:
        connected_clients.discard(websocket)
        logging.info("Browser disconnected (total: %d)", len(connected_clients))


async def broadcast(msg: str) -> None:
    if connected_clients:
        await asyncio.gather(
            *[ws.send(msg) for ws in list(connected_clients)],
            return_exceptions=True,
        )

# ── Single UDP socket (send + receive) ───────────────────────────────────────
# The Ada server reads the sender address from each Request_Records packet,
# replaces the port with Response_Port (50004), and sends Status_Records back
# to that address. So we must send from port 50004 — the same socket both
# sends requests and receives responses.

async def udp_receive_loop(udp: socket.socket) -> None:
    loop = asyncio.get_running_loop()
    logging.info("Listening for clock status on UDP port %d", RESPONSE_PORT)
    while True:
        try:
            data = await loop.sock_recv(udp, 4096)
            payload = decode_status(data)
            await broadcast(json.dumps(payload))
        except ValueError as exc:
            logging.warning("Decode error: %s", exc)
        except Exception as exc:
            logging.error("UDP receive error: %s", exc)


async def poll_clock(udp: socket.socket) -> None:
    """Send Get_Status at 8 Hz to keep the server responding."""
    while True:
        try:
            pkt = encode_request("Get_Status", ambient_override=_ambient_override)
            udp.sendto(pkt, (CLOCK_HOST, REQUEST_PORT))
        except Exception:
            pass
        await asyncio.sleep(0.125)

# ── Entry point ───────────────────────────────────────────────────────────────
async def main() -> None:
    global _udp_tx
    logging.basicConfig(level=logging.INFO,
                        format="%(asctime)s %(levelname)s %(message)s")
    logging.info("Status_Records size: %d bytes", STATUS_SIZE)
    logging.info("Clock host: %s", CLOCK_HOST)

    # Bind to RESPONSE_PORT so Ada sends replies back to us on this port
    _udp_tx = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    _udp_tx.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    _udp_tx.bind(("0.0.0.0", RESPONSE_PORT))
    _udp_tx.setblocking(False)

    logging.info("WebSocket server: ws://%s:%d", WS_HOST, WS_PORT)
    logging.info("Forwarding requests → %s:%d", CLOCK_HOST, REQUEST_PORT)

    if HTTP_PORT > 0:
        logging.info("Starting HTTP file server on port=%d", HTTP_PORT)
        threading.Thread(target=_start_http_server, args=(HTTP_PORT,), daemon=True).start()

    async with serve(ws_handler, WS_HOST, WS_PORT):
        await asyncio.gather(udp_receive_loop(_udp_tx), poll_clock(_udp_tx))


if __name__ == "__main__":
    asyncio.run(main())
