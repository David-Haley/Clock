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

# On Git Bash for Windows (MSYS), docker.exe is a native Windows binary, so
# MSYS rewrites Unix-style absolute-path arguments into Windows paths before
# they arrive -- including /build/... arguments that are meant to be resolved
# inside the container, not on the host. It also mis-parses "-v host:/build"
# volume specs as a PATH list when conversion is only partially disabled.
# Disabling conversion entirely (MSYS_NO_PATHCONV) avoids both problems, but
# then host paths must already be in native Windows form -- `pwd -W` (an
# MSYS-only extension) provides that. No effect on real Linux/macOS bash,
# where OSTYPE is never "msys".
if [[ "$OSTYPE" == "msys" ]]; then
    export MSYS_NO_PATHCONV=1
    PWD_FLAGS=(-W)
else
    PWD_FLAGS=()
fi

CONTAINER_NAME=iot-clock-sim
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd "${PWD_FLAGS[@]}")"
PARENT="$(cd "$SCRIPT_DIR/../.." && pwd "${PWD_FLAGS[@]}")"

# ── Detect the host's timezone so the simulator's primary display shows local
# time rather than UTC (the container has no timezone of its own). Override by
# exporting TZ before running this script, e.g. TZ=Australia/Sydney ./docker/run_sim.sh
detect_timezone() {
    if [ -n "${TZ:-}" ]; then
        echo "$TZ"
    elif [ -r /etc/timezone ]; then
        cat /etc/timezone
    elif [ -e /etc/localtime ]; then
        resolved="$(readlink -f /etc/localtime 2>/dev/null || true)"
        case "$resolved" in
            */zoneinfo/*) echo "${resolved#*/zoneinfo/}" ;;
            *) echo "Etc/UTC" ;;
        esac
    else
        echo "Etc/UTC"
    fi
}

SIM_TZ="$(detect_timezone)"
if [ "$SIM_TZ" = "Etc/UTC" ] && [ -z "${TZ:-}" ]; then
    echo "Warning: could not detect host timezone, simulator will use UTC." \
         "Override with TZ=Region/City ./docker/run_sim.sh"
fi

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
echo "  Simulator WebSocket: ws://localhost:8765"
echo "  Simulator HTTP UI:   http://localhost:8080/index.html"
echo "  Timezone:            $SIM_TZ (override with TZ=Region/City ./docker/run_sim.sh)"
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
    --dns-search 19bluebell4161.net.au \
    -e "TZ=$SIM_TZ" \
    -v "$PARENT:/build" \
    -w /build/Clock \
    -p 8765:8765/tcp \
    -p 8080:8080/tcp \
    iot-clock-build \
    /build/Clock/docker/entrypoint.sh
