#!/usr/bin/env bash
# Run the WebSocket bridge for a real physical clock on the LAN.
#
# Usage:
#   ./web/run_bridge.sh <clock-host>
#
# Opens the web UI on HTTP port 8000 and the WebSocket bridge on port 8765.
# On another machine on the same LAN, open:
#   http://<this-machine-ip>:8000/index.html
#
# The clock must have UDP ports 50003 and 50004 reachable from this machine.
set -e

CLOCK_HOST="${1:?Usage: $(basename "$0") <clock-host>}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Connecting to clock at: ${CLOCK_HOST}"
echo "Web UI:                 http://localhost:8000/index.html"
echo "WebSocket:              ws://0.0.0.0:8765"
echo "Press Ctrl-C to stop."
echo ""

BRIDGE_HTTP_PORT=8000 uv run --with websockets "$SCRIPT_DIR/bridge.py" "$CLOCK_HOST"
