# IOT_Clock
Software for a clock based on purpose built hardware driven by a Raspberry Pi 3B.

## Hardware
 The hardware consists of two six digit seven segment displays (0.8" high) and sixty four individual LEDs (5mm) to provide a simulated sweephand with reference markers at 0, 15 ,30 and 45.
 The hardware suports automatic brightness control over a 4095 to one range with 64 levels of analogue brightness compensation to match the brightness of individual LEDs and segments.
 The clock can chime by playing arbitrary .wav files (uses amixer and aplay).

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
| `Pi_Common_C` | C SPI/I2C/GPIO low-level drivers |

Expected directory layout:
```
parent/
├── Clock/          ← this repository
├── DJH/
├── Pi_Common/
└── Pi_Common_C/
```

## Runtime Configuration Files

The following CSV files must be present in the working directory when the clock is started:
- General configuration (brightness thresholds, sweep mode, gamma correction, audio command paths)
- Brightness calibration (per-LED dot correction values — 160 values across 10 TLC5940 drivers)
- Chime schedule (hour → `.wav` file path mapping; missing entries silence that hour)

WAV files referenced by the chime schedule must also be accessible at the configured paths.
