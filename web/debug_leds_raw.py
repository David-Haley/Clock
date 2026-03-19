#!/usr/bin/env python3
"""
Raw LED + status dump from the IOT Clock WebSocket bridge.
Prints decoded fields including the full 10x16 LED array.

Usage:
    uv run --with websockets debug_leds_raw.py [ws://localhost:8765] [--watch]
"""

import asyncio
import json
import sys

import websockets

WS_URL = "ws://localhost:8765"
WATCH = False

for arg in sys.argv[1:]:
    if arg.startswith("ws://"):
        WS_URL = arg
    elif arg == "--watch":
        WATCH = True

DRIVER_NAMES = [
    "Sweep_00_14", "Sweep_15_29", "Sweep_30_44", "Sweep_45_59",
    "Seconds_Drv", "Minutes_Drv", "Hours_Drv",
    "Years_Drv",   "Months_Drv",  "Days_Drv",
]


def print_status(data: dict, n: int = 0) -> None:
    print(f"\n─── Frame {n} ─────────────────────────────────────────────────")
    print(f"  time         : {data.get('time')!r}  (note: may be bridge fallback if Ada Current_Time=0)")
    print(f"  version      : {data.get('clock_version')!r}")
    print(f"  ui_version   : {data.get('ui_version')!r}")
    print(f"  chime        : enabled={data.get('chime_enabled')}  toggle={data.get('chime_toggle')}  vol={data.get('volume')}%")
    print(f"  ambient_light: {data.get('ambient_light')}  al_test={data.get('al_test_value')}")
    print(f"    (ambient_light=0 AND time is current → main loop has NOT run yet)")
    print(f"  secondary    : {data.get('current_item', '').strip()!r}")

    leds = data.get("leds", [])
    total_lit = sum(1 for row in leds for v in row if v)
    total_ch = len(leds) * (len(leds[0]) if leds else 0)
    print(f"  lit LEDs     : {total_lit} / {total_ch}")
    for r, row in enumerate(leds):
        name = DRIVER_NAMES[r] if r < len(DRIVER_NAMES) else f"row{r}"
        bits = [int(v) for v in row]
        lit = [i for i, v in enumerate(bits) if v]
        status = f"ch {lit}" if lit else "(all off)"
        print(f"    [{r}] {name:16s}: {status}")


async def run() -> None:
    print(f"Connecting to {WS_URL} …")
    async with websockets.connect(WS_URL) as ws:
        print("Connected.")
        n = 0
        async for raw in ws:
            data = json.loads(raw)
            print_status(data, n)
            n += 1
            if not WATCH:
                break


asyncio.run(run())
