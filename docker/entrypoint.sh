#!/usr/bin/env bash
# Runs inside the Docker container: starts the Ada clock then the bridge.
# Bridge connects to Ada via loopback (127.0.0.1), bypassing Docker NAT.
# WebSocket is exposed on 0.0.0.0:8765 so Docker can forward it to the Mac.
set -e

CLOCK_BIN=/build/Clock/obj_sim/iot_clock
BRIDGE=/build/Clock/web/bridge.py

"$CLOCK_BIN" &
CLOCK_PID=$!
echo "Clock started (PID $CLOCK_PID)"

cleanup() {
    echo "Stopping clock..."
    kill "$CLOCK_PID" 2>/dev/null
    wait "$CLOCK_PID" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

# Brief pause so Ada's UDP socket is open before bridge starts polling
sleep 0.5

BRIDGE_WS_HOST=0.0.0.0 python3 "$BRIDGE" 127.0.0.1
