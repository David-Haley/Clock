#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PARENT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Ensure Colima is running with the aarch64 profile
if ! colima status --profile aarch64 2>/dev/null | grep -q "Running"; then
  echo "Starting Colima aarch64 profile..."
  colima start --profile aarch64 --arch aarch64 --vm-type vz --vz-rosetta
fi

echo "Building Docker image iot-clock-build..."
docker build --platform linux/arm64 \
  -f "$SCRIPT_DIR/Dockerfile" \
  -t iot-clock-build \
  "$PARENT"

echo "Running production build (iot_clock.gpr)..."
docker run --rm --platform linux/arm64 \
  -v "$PARENT:/build" \
  iot-clock-build

echo "Build complete. Binaries in Clock/obj/"
