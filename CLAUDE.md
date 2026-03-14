# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

IOT_Clock is an Ada 2022 application for a Raspberry Pi 3B-based digital clock with:
- Two six-digit 7-segment displays
- 64 individual LEDs simulating a sweep hand (with markers at 0/15/30/45 seconds)
- Automatic brightness control (4095:1 range, 64 levels of per-LED dot correction)
- Hourly chiming via arbitrary `.wav` files (`aplay` + `amixer`)
- Distributed user interface via TCP sockets

## Build Commands

The project uses GNAT/GPRBuild with project file `iot_clock.gpr`.

```bash
# Build all targets
gprbuild -P iot_clock.gpr

# Build a specific target
gprbuild -P iot_clock.gpr iot_clock.adb
gprbuild -P iot_clock.gpr clock_ui.adb
gprbuild -P iot_clock.gpr test_clock.adb
gprbuild -P iot_clock.gpr test_chime.adb

# Clean build artifacts
gprclean -P iot_clock.gpr
```

Object files are placed in `obj/`. Build flags: `-gnat2022 -g -gnato -fstack-check -gnatE -gnatf -gnatVd`.

To release GPIO pins on exit: `release_gpio.sh`

## External Dependencies (sibling directories)

The `.gpr` file references three sibling repositories that must exist alongside this one:

- `../DJH/src` — `Events_and_Errors` (logging) and `Parse_CSV` (CSV config parsing)
- `../Pi_Common/src` — Ada drivers: `RPi_GPIO`, `TLC5940`, `Linux_Signals`
- `../Pi_Common_C/src` — C SPI interface for TLC5940 chips

## Architecture

### Main Loop (`src/iot_clock.adb`)
Runs at 1 Hz for second-level time updates, with an inner loop at 8/16/60 Hz depending on sweep mode. Coordinates all subsystems: display updates, sweep animation, brightness measurement, chiming, NTP correction, and IPC server.

### Hardware Abstraction
- **`src/clock_driver.ads`** — Instantiates the generic `TLC5940` driver for 10 chips (160 channels total). This is the single point of hardware coupling.
- **`src/led_declarations.ads`** — Maps logical display elements (digits, sweep LEDs, markers, ambient light sensor) to specific TLC5940 driver/channel indices. Change hardware wiring here.

### Display System
- **Primary display**: Hours:Minutes:Seconds on the first six digits
- **Secondary display** (`src/secondary_display.adb`): Cycles through day/month/year; also shows configuration on startup
- Segments are driven via `Segment_Array` arrays defined in `led_declarations.ads`

### Configuration (CSV files, read at startup)
- **General config** (`src/general_configuration.adb`): minimum brightness, chiming threshold, sweep mode, gamma correction, default volume, paths to `aplay`/`amixer`
- **Brightness/dot correction** (`src/brightness.adb`): per-LED calibration factors (160 values, 10 drivers × 16 channels)
- **Chime schedule** (`src/chime.adb`): up to 24 entries mapping hours to `.wav` file paths; missing entries silence that hour

### User Interface (TCP IPC)
- **Server** (`src/user_interface_server.adb`): runs inside the clock process, listens on ports 50003/50004
- **Client** (`src/clock_ui.adb` + `src/user_interface_client.adb`): standalone interactive UI; takes clock hostname as argument
- Protocol defined in `src/shared_user_interface.ads` (versioned Request/Response records streamed over sockets)
- Client commands: toggle chime, cycle sweep mode, volume up/down, volume test

### Sweep Modes
`Normal`, `Smooth`, `With_Tail`, `Sub_Second` — controlled via general configuration or UI at runtime.

## Ada-Specific Notes

- Ada 2022 features are used; GNAT toolchain required
- Elaboration issues can be debugged by uncommenting the `Binder` package in `iot_clock.gpr`
- The project compiles both Ada and C (C is used only for the low-level SPI interface in `Pi_Common_C`)
