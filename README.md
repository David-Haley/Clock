# IOT_Clock
Software for a clock based on purpose built hardware driven by a Raspberry Pi 3B.

## Hardware
 The hardware consists of two six digit seven segment displays (0.8" high) and sixty four individual LEDs (5mm) to provide a simulated sweephand with reference markers at 0, 15 ,30 and 45.
 The hardware suports automatic brightness control over a 4095 to one range with 64 levels of analogue brightness compensation to match the brightness of individual LEDs and segments.
 The clock can chime by playing arbitrary .wav files (uses amixer and aplay).

## Hardware Target

The hardware binary targets a Raspberry Pi 3B running Raspberry Pi OS (Linux, aarch64). The physical clock is built around this platform. Other Pi models with compatible GPIO/SPI/I2C pin layouts may work, but have not been tested.

The C drivers require Linux-specific kernel interfaces:
- `/dev/gpiochip0` — GPIO via libgpiod
- `/dev/spidev0.0`, `/dev/spidev0.1` — SPI for TLC5940 LED drivers and ADC
- `/dev/i2c-0`, `/dev/i2c-1` — I2C for display controller

These do not exist on macOS or Windows. Use the [simulator](#running-the-simulator) to run on non-Pi hardware.

## Build Prerequisites (on Raspberry Pi OS)

Install the following prerequisites:

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

The following CSV files must be present in the `Clock/` working directory when the clock binary is started. Missing files cause silent failures — see Troubleshooting above.

| File | Required | Purpose |
|------|----------|---------|
| `General_Configuration.csv` | Yes | Minimum brightness, chime threshold, sweep mode, gamma, volume, audio command paths |
| `Brightness.csv` | Yes | Per-LED dot correction — 160 values (10 TLC5940 drivers × 16 channels). Missing this file causes elaboration failure; child tasks survive but the main loop never starts |
| `Chimes.csv` | No | Hour → `.wav` file path mapping; missing entries silence that hour |
| `Secondary.csv` | No | Secondary display configuration; missing shows blank secondary |

WAV files referenced by `Chimes.csv` must also be accessible at the configured paths.

### Example Configuration Files

`Example_Configuration/` contains reference files from the real hardware build, useful as a starting point:

| File | Notes |
|------|-------|
| `Brightness.csv` | Per-LED calibration from real hardware (mostly 50, with one channel at 10 for the ambient light sensor input) |
| `General_Configuration.csv` | Real hardware settings: `aplay`/`amixer` paths, `Minimum_Chime` of `6` (chime unless very dark) |
| `Chimes.csv` | Chime schedule from real hardware |
| `Secondary.csv` | Secondary display config from real hardware |
| `iot_clock.service` | systemd service unit for auto-start on boot |

### Simulator vs Hardware Differences

Two config files need different values for the simulator:

**`General_Configuration.csv`**
- `Play_Command` / `Volume_Command`: hardware uses `/usr/bin/aplay -q` and `/usr/bin/amixer ...`; simulator uses `echo` as a no-op stub (no audio hardware)
- `Minimum_Chime`: a `Greyscales` value (0–4095) representing the ambient light level above which chiming is allowed. Hardware uses `6` (chime unless very dark); simulator uses `4095` (effectively disabled — no real ambient sensor)
- Use `Example_Configuration/General_Configuration.sim.csv` as a starting point for simulator use

**`Brightness.csv`**
- Hardware uses per-LED calibrated values; simulator uses uniform `31` (uncalibrated default — all LEDs equal)
- Use `Example_Configuration/Brightness.sim.csv` as a starting point for simulator use

To set up the simulator manually:
```bash
cp Example_Configuration/General_Configuration.sim.csv General_Configuration.csv
cp Example_Configuration/Brightness.sim.csv Brightness.csv
```
The Docker scripts (`docker/run_sim.sh`) handle this automatically.
