#!/usr/bin/env bash
# Build the simulation binary and run it inside Docker.
#
# The bridge runs INSIDE the container alongside Ada, communicating via
# loopback (no Docker UDP NAT issues). Only the WebSocket port (TCP 8765)
# is forwarded to the Mac — TCP forwarding works reliably with Colima.
#
# Usage:
#   ./docker/run_sim.sh          # build + run (blocks; Ctrl-C to stop)
#   docker stop iot-clock-sim    # stop from another terminal
#
# Then open Clock/web/index.html in a browser.
set -e

CONTAINER_NAME=iot-clock-sim
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PARENT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# ── Stop any existing simulator container ────────────────────────────────────
if docker ps -q --filter "name=^${CONTAINER_NAME}$" | grep -q .; then
    echo "Stopping existing ${CONTAINER_NAME} container..."
    docker stop "$CONTAINER_NAME" 2>/dev/null || true
fi

# ── Ensure Colima is running (macOS only) ────────────────────────────────────
if command -v colima &>/dev/null; then
    if ! colima status --profile aarch64 2>/dev/null | grep -q "Running"; then
        echo "Starting Colima aarch64 profile..."
        colima start --profile aarch64 --arch aarch64 --vm-type vz --vz-rosetta
    fi
fi

# ── Build Docker image ───────────────────────────────────────────────────────
docker build --platform linux/arm64 \
    -f "$SCRIPT_DIR/Dockerfile" \
    -t iot-clock-build \
    "$PARENT"

# ── Build Ada simulation binary ──────────────────────────────────────────────
echo "Building simulation binary (iot_clock_sim.gpr)..."
docker run --rm --platform linux/arm64 \
    -v "$PARENT:/build" \
    iot-clock-build \
    gprbuild -P /build/Clock/iot_clock_sim.gpr -j0

echo ""
echo "Simulation binary built at Clock/obj_sim/iot_clock"
echo "Running clock + bridge..."
echo "  Simulator WebSocket: ws://localhost:8765  (or open web/index.html as a file)"
echo "  For a real clock:    ./web/run_bridge.sh <clock-host>"
echo "  Stop with: docker stop ${CONTAINER_NAME}  (or Ctrl-C)"
echo ""

# ── Run Ada clock + bridge together ─────────────────────────────────────────
# Bridge runs inside the container so it talks to Ada via 127.0.0.1 (no NAT).
# Only TCP port 8765 (WebSocket) is forwarded to the Mac; the bridge binds
# to all interfaces inside the container (Docker controls external access).
docker run --rm \
    --name "$CONTAINER_NAME" \
    --platform linux/arm64 \
    -v "$PARENT:/build" \
    -w /build/Clock \
    -p 8765:8765/tcp \
    iot-clock-build \
    /build/Clock/docker/entrypoint.sh
