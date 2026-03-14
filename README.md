# IOT_Clock
Software for a clock based on purpose built hardware driven by a Raspberry Pi 3B.

## Hardware
 The hardware consists of two six digit seven segment displays (0.8" high) and sixty four individual LEDs (5mm) to provide a simulated sweephand with reference markers at 0, 15 ,30 and 45.
 The hardware suports automatic brightness control over a 4095 to one range with 64 levels of analogue brightness compensation to match the brightness of individual LEDs and segments.
 The clock can chime by playing arbitrary .wav files (uses amixer and aplay).

<<<<<<< HEAD
## Platform Requirement

**This project can only be compiled and run on a Raspberry Pi 3B running Linux (Raspberry Pi OS / Raspbian).**

The C drivers use Linux-specific kernel interfaces that do not exist on macOS or Windows:
- `/dev/gpiochip0` — GPIO via libgpiod
- `/dev/spidev0.0`, `/dev/spidev0.1` — SPI for TLC5940 LED drivers and ADC
- `/dev/i2c-0`, `/dev/i2c-1` — I2C for display controller
- Kernel headers: `linux/spi/spidev.h`, `linux/i2c-dev.h`

## Build Prerequisites (on Raspberry Pi OS)

Install the following before attempting to build:

```bash
# Ada 2022 compiler and GPRBuild
sudo apt install gnat gprbuild

# libgpiod development headers and shared library (required for GPIO control)
sudo apt install libgpiod-dev

# Linux kernel headers (required to compile SPI/I2C C drivers)
sudo apt install linux-headers-$(uname -r)

# ALSA utilities (required at runtime for chiming)
sudo apt install alsa-utils
```

Enable the SPI and I2C interfaces via `raspi-config` → Interface Options, or add to `/boot/config.txt`:
```
dtparam=spi=on
dtparam=i2c_arm=on
```

## Sibling Repositories

The following repositories must be cloned alongside this one (i.e. all four must share the same parent directory):

| Repository | Used for |
|------------|---------|
| `DJH` | `Events_and_Errors` (logging) and `Parse_CSV` (CSV config parsing) |
| `Pi_Common` | Ada drivers: `RPi_GPIO`, `TLC5940`, `Linux_Signals` |
| `Pi_Common_C` | C low-level drivers: `SPI_interface`, `gpio_driver` |

Expected directory layout:
```
parent/
├── Clock/          ← this repository
├── DJH/
├── Pi_Common/
└── Pi_Common_C/
```

## Cross-Compiling for the Raspberry Pi

If you want to build the production binary on your Mac (without a Pi connected), use the Docker-based cross-compiler:

```bash
# Build the arm64 production binary inside Docker
docker/build.sh

# Output binary is at Clock/obj/iot_clock — copy it to the Pi:
scp Clock/obj/iot_clock pi@<pi-hostname>:~/
```

This compiles `iot_clock.gpr` (real hardware drivers) inside an arm64 Ubuntu container, so the result runs natively on the Pi without any stubs.

> **When to use this vs the simulator:** Use `docker/build.sh` when you're ready to deploy to real hardware. Use `docker/run_sim.sh` during development to test display logic in a browser without needing a Pi.

## Running the Simulator

The clock can be run on any machine using Docker, without real hardware. The simulator uses stub C drivers (no GPIO/SPI access) and streams the display state to a browser via WebSocket.

### Prerequisites

| Platform | Requirements |
|----------|-------------|
| **macOS** | [Docker Desktop](https://www.docker.com/products/docker-desktop/) + [Colima](https://github.com/abiosoft/colima) (`brew install colima`) for arm64 emulation |
| **Windows** | [Docker Desktop](https://www.docker.com/products/docker-desktop/) with the WSL 2 backend enabled; run all commands from a WSL 2 terminal |
| **Linux** | Docker Engine with `binfmt_misc` + QEMU for arm64 (`docker run --rm --privileged multiarch/qemu-user-static --reset -p yes`) |

All platforms require the four sibling repositories cloned into the same parent directory (see [Sibling Repositories](#sibling-repositories) below).

### Quick Start

```bash
# From the Clock/ directory — builds the image, compiles the sim binary, and starts it
docker/run_sim.sh

# Then open the web UI in a browser:
#   macOS/Linux: open web/index.html  (or file:///path/to/Clock/web/index.html)
#   Windows:     open web\index.html in Explorer, or use the file:// URL in a browser

# Stop the simulator
docker stop iot-clock-sim
```

The script blocks in the foreground. Ctrl-C stops it, or run `docker stop iot-clock-sim` from another terminal.

> **Windows note:** `run_sim.sh` is a Bash script and uses `colima` (macOS only). Run it from a **WSL 2 terminal**. The Colima section will silently no-op since Docker Desktop's WSL 2 backend already provides the Linux VM — arm64 emulation via QEMU is handled automatically.

### How It Works

- The Ada binary is compiled inside the Docker image using `iot_clock_sim.gpr` (stub drivers, no hardware access)
- A Python WebSocket bridge (`web/bridge.py`) runs **inside** the container alongside Ada, communicating over loopback — this avoids Docker UDP NAT issues
- Only TCP port 8765 (WebSocket) is forwarded to the host
- `web/index.html` connects to `ws://localhost:8765` and renders the clock in the browser

### Troubleshooting

- **Display not updating / all LEDs off:** Check `Error_Log.txt` in the Clock directory. A missing config file (most commonly `Brightness.csv`) causes the Ada main task to fail silently while child tasks keep running.
- **Connection refused on port 8765:** The container may still be starting. Wait a few seconds and reload.
- **Logs:** `docker logs iot-clock-sim` shows Ada stdout and bridge output.

## Runtime Configuration Files

The following CSV files must be present in the working directory when the clock is started:
- General configuration (brightness thresholds, sweep mode, gamma correction, audio command paths)
- Brightness calibration (per-LED dot correction values — 160 values across 10 TLC5940 drivers)
- Chime schedule (hour → `.wav` file path mapping; missing entries silence that hour)

WAV files referenced by the chime schedule must also be accessible at the configured paths.

Sample config files are provided in `Example_Configuration/` — these are from real hardware and serve as a starting point. The simulator requires its own `General_Configuration.csv` with `echo` substituted for `aplay`/`amixer` (the Docker scripts handle this automatically).

## Files Added in This PR

| Path | Description |
|------|-------------|
| `docker/` | Dockerfile, `run_sim.sh` (build + run), `build.sh` (cross-compile only), `entrypoint.sh` |
| `web/bridge.py` | Python WebSocket bridge; runs inside the container, forwards LED state to the browser on port 8765 |
| `web/index.html` | Browser clock UI; connects to `ws://localhost:8765` and renders the display |
| `web/debug_*.py` | Utilities for inspecting raw WebSocket output |
| `src_sim/` | Ada stubs replacing the real Linux signal handler; required to avoid GNAT interrupt-priority elaboration errors when building without hardware headers |
| `iot_clock_sim.gpr` | Simulator GPR project (uses stub drivers from `src_sim/` instead of hardware drivers) |
| `Example_Configuration/` | Real hardware CSV config files (`Brightness.csv`, `General_Configuration.csv`, `Chimes.csv`, `Secondary.csv`) and a systemd service unit (`iot_clock.service`); useful as a starting point for hardware setup |
| `libgpiod.gpr` | GPR project wrapping the system libgpiod library |
