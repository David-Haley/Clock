#!/usr/bin/env python3
"""
Dumps raw UDP bytes received from the Ada clock simulator (bypassing the
WebSocket bridge). Sends a Get_Status request directly to port 50003 and
prints the response as hex + annotated field offsets.

Usage (from Clock/web):
    uv run debug_raw_bytes.py [host] [--count N]

Defaults: host=127.0.0.1, count=1
"""

import socket
import struct
import sys

HOST  = "127.0.0.1"
COUNT = 1

for arg in sys.argv[1:]:
    if arg.startswith("--count"):
        COUNT = int(arg.split("=")[-1]) if "=" in arg else int(sys.argv[sys.argv.index(arg)+1])
    elif not arg.startswith("--"):
        HOST = arg

REQUEST_FMT = "<8sI?3x"
STATUS_SIZE_EXPECTED = 284

def send_get_status(sock):
    pkt = struct.pack(REQUEST_FMT, b"20250510", 5, False)   # Get_Status = 5
    sock.sendto(pkt, (HOST, 50003))

def hexdump(data, width=16):
    for i in range(0, len(data), width):
        chunk = data[i:i+width]
        hex_part  = " ".join(f"{b:02x}" for b in chunk)
        ascii_part = "".join(chr(b) if 32 <= b < 127 else "." for b in chunk)
        print(f"  {i:4d}  {hex_part:<{width*3}}  {ascii_part}")

# Field layout for annotation
FIELDS = [
    (0,   8,  "Clock_Version (String 1..8)"),
    (8,   4,  "Request (enum uint32)"),
    (12,  1,  "Diagnostic_Toggle (bool)"),
    (13,  8,  "User_Interface_Version (String 1..8)"),
    (21,  3,  "padding (3x)"),
    (24,  8,  "Current_Time (int64 ns)"),
    (32,  1,  "Chime_Enabled (bool)"),
    (33,  1,  "Chime_Toggle (bool)"),
    (34,  2,  "padding (2x)"),
    (36,  4,  "Chime_Volume (int32)"),
    (40,  2,  "Ambient_Light (uint16)"),
    (42,  2,  "AL_Test_Value (uint16)"),
    (44, 80,  "Current_Item (String 1..80)"),
    (124,160, "LED_Array (160 bool)"),
]

def annotate(data):
    print(f"\nAnnotated fields ({len(data)} bytes):")
    for off, size, name in FIELDS:
        chunk = data[off:off+size]
        hex_s = " ".join(f"{b:02x}" for b in chunk[:min(size,20)])
        extra = "..." if size > 20 else ""
        # Try to decode as useful type
        note = ""
        if "String" in name:
            note = repr(chunk.decode("ascii", errors="replace"))
        elif size == 8 and "Time" in name:
            ns = struct.unpack_from("<q", chunk)[0]
            note = f"{ns} ns"
        elif size == 4:
            note = str(struct.unpack_from("<i", chunk)[0])
        elif size == 2:
            note = str(struct.unpack_from("<H", chunk)[0])
        elif size == 1:
            note = str(chunk[0])
        elif "LED" in name:
            lit = sum(1 for b in chunk if b)
            note = f"{lit}/160 lit"
        print(f"  off {off:3d} [{size:3d}B]  {hex_s}{extra}  {name}  {note}")

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.settimeout(2.0)
sock.bind(("0.0.0.0", 50004))

for i in range(COUNT):
    send_get_status(sock)
    try:
        data, addr = sock.recvfrom(4096)
        print(f"\n=== Response {i+1} from {addr} ({len(data)} bytes) ===")
        hexdump(data)
        annotate(data)
    except socket.timeout:
        print("TIMEOUT — no response (is the simulator running?)")
        break

sock.close()
