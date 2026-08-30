# IOT_Clock
Software for a clock based on purpose built hardware driven by a Raspberry Pi 3B.

## Hardware
 The hardware consists of two six digit seven segment displays (0.8" high) and sixty four individual LEDs (5mm) to provide a simulated sweephand with reference markers at 0, 15 ,30 and 45.
 The hardware suports automatic brightness control over a 4095 to one range with 64 levels of analogue brightness compensation to match the brightness of individual LEDs and segments.
 The clock can chime by playing arbitrary .wav files (uses amixer and aplay).

## Hardware Target

The hardware binary targets a Raspberry Pi 3B running Raspberry Pi OS (Linux, aarch64). The physical clock is built around this platform. Other Pi models with compatible GPIO/SPI pin layouts may work, but have not been tested.

The C drivers require Linux-specific kernel interfaces:
- `/dev/gpiochip0` — GPIO via libgpiod
- `/dev/spidev0.0`, `/dev/spidev0.1` — SPI for TLC5940 LED drivers and ADC

These do not exist on macOS or Windows. Use the [simulator](#running-the-simulator) to run on non-Pi hardware.

> `Pi_Common`/`Pi_Common_C` also include an I2C driver and display binding (`i2c_interface`,
> `dfr0555_display`), used by other projects sharing those sibling repositories. `iot_clock`
> does not `with` them and has no I2C dependency.

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

# Mosquitto client library headers (required to compile MQTT support)
sudo apt install libmosquitto-dev
```

Enable the SPI interface via `raspi-config` → Interface Options, or add to `/boot/config.txt`:
```
dtparam=spi=on
```
(`Pi_Common_C/src` also contains an I2C driver compiled alongside the SPI/GPIO drivers — the
kernel headers above cover it — but `iot_clock` never opens `/dev/i2c-*`, so the I2C
interface does not need to be enabled.)

## Sibling Repositories

The following repositories must be cloned alongside this one (i.e. all four must share the same parent directory):

| Repository | Used for |
|------------|---------|
| `DJH` | `Events_and_Errors` (logging) and `Parse_CSV` (CSV config parsing) |
| `Pi_Common` | Ada drivers: `RPi_GPIO`, `TLC5940`, `Linux_Signals`, `MQTT_Client` (libmosquitto binding); also the GNATCOLL and mosquitto `.gpr` files used for JSON parsing and MQTT |
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

The following files must be present in the `Clock/` working directory when the clock binary is started. Missing required files cause silent failures — see Troubleshooting above.

| File | Format | Required | Purpose |
|------|--------|----------|---------|
| `General_Configuration.csv` | CSV | Yes | Minimum brightness, chime threshold, sweep mode, gamma, volume, audio command paths |
| `Brightness.csv` | CSV | Yes | Per-LED dot correction — 160 values (10 TLC5940 drivers × 16 channels). Missing this file causes elaboration failure; child tasks survive but the main loop never starts |
| `Chimes.csv` | CSV | No | Hour → `.wav` file path mapping; missing entries silence that hour |
| `Secondary.json` | JSON | No | Ordered list of secondary display items (date formats, second time zone, static/scrolling text, MQTT values, arbitrary segments); missing shows blank secondary. Replaces the old `Secondary.csv` format |
| `Topic_Management.json` | JSON | No | MQTT broker subscriptions and the topic/field mapping for `MQTT` items shown on the secondary display; missing disables MQTT display items. Edit with the `topic_editor` tool rather than by hand — see [Topic Management](#topic-management-mqtt) below |

WAV files referenced by `Chimes.csv` must also be accessible at the configured paths.

### Example Configuration Files

`Example_Configuration/` contains reference files from the real hardware build, useful as a starting point:

| File | Notes |
|------|-------|
| `Brightness.csv` | Per-LED calibration from real hardware (mostly 50, with one channel at 10 for the ambient light sensor input) |
| `General_Configuration.csv` | Real hardware settings: `aplay`/`amixer` paths, `Minimum_Chime` of `6` (chime unless very dark) |
| `Chimes.csv` | Chime schedule from real hardware |
| `Secondary.json` | Secondary display config from real hardware |
| `Topic_Management.json` | MQTT subscription/item config from real hardware (passwords are lightly encrypted, not plaintext-safe to share) |
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

## Topic Management (MQTT)

Secondary display items of type `MQTT` (in `Secondary.json`) show a value read from an MQTT
topic. Which broker to subscribe to, the login credentials, and how each item's value is
extracted and formatted are stored in `Topic_Management.json`, read once at startup.

`topic_editor` exists because that file has two nested layers that don't map neatly to one
simple list: a handful of **topic subscriptions** (broker + credentials), each carrying a
JSON payload with multiple fields, and separately a set of **display items**, each pulling
one field out of one subscribed topic and formatting it for the six-digit display. The
example in `Example_Configuration/Topic_Management.json` shows this in practice — a single
broker (`Domain-Controller`, a home-automation server) with two topic subscriptions,
`Hot_Water_Status` (field `Tank_Temperature`, shown as `Hot_Water_Temperature`) and `PV_Data`
(fields `power` and `daily_yeild`, shown as `PV_Power` and `PV_Yeild`) — i.e. the clock is
pulling live hot-water and solar-PV readings from a home-automation MQTT broker. Hand-editing
the raw JSON is error-prone once formatting rules and multiple items per topic are involved,
so `topic_editor` provides validated add/modify/delete operations instead.

This file is not intended to be hand-edited — build and run the interactive `topic_editor`
tool instead:

```bash
gprbuild -P iot_clock.gpr topic_editor.adb   # or iot_clock_sim.gpr in the simulator
./obj/topic_editor
```

`topic_editor` has two modes, reached from its top-level menu:
- **T**opic (subscription) editor — add/modify/delete a broker subscription (topic, broker
  hostname, user, password) and verify a stored password
- **I**tem editor — add/modify/delete an `MQTT_Item_Id` (the id referenced by `Item_Id` in
  `Secondary.json`), mapping it to a topic + field and a formatting rule: string, plain
  number, scaled unsigned 16/32-bit (with scaling factor, decimal places, and signed/unsigned
  interpretation), or boolean (with custom true/false display text)

Changes are held in memory until **S**ave writes `Topic_Management.json`; **Q**uit discards
unsaved changes.
