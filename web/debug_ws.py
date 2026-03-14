#!/usr/bin/env python3
"""
Debug script for the IOT Clock WebSocket bridge.

Usage:
    uv run --with websockets debug_ws.py [ws://localhost:8765] [--watch]

Options:
    --watch     Keep receiving and printing updates (default: single snapshot)
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

LED_DRIVER_NAMES = [
    "Sweep_00_14", "Sweep_15_29", "Sweep_30_44", "Sweep_45_59",
    "Seconds_Drv", "Minutes_Drv", "Hours_Drv",
    "Years_Drv",   "Months_Drv",  "Days_Drv",
]


def print_status(data: dict, n: int = 0) -> None:
    print(f"\n─── Frame {n} ─── {data.get('time', '?')} ───────────────────────")
    print(f"  version      : {data.get('clock_version')}")
    print(f"  chime        : enabled={data.get('chime_enabled')}  toggle={data.get('chime_toggle')}  vol={data.get('volume')}%")
    print(f"  ambient      : {data.get('ambient_light')}  al_test={data.get('al_test_value')}")
    print(f"  secondary    : {data.get('current_item', '').strip()!r}")

    leds = data.get("leds", [])
    total_lit = sum(1 for row in leds for v in row if v)
    print(f"  lit LEDs     : {total_lit} / {len(leds) * (len(leds[0]) if leds else 0)}")
    for r, row in enumerate(leds):
        ones = [i for i, v in enumerate(row) if v]
        name = LED_DRIVER_NAMES[r] if r < len(LED_DRIVER_NAMES) else f"row{r}"
        if ones:
            print(f"    [{r}] {name:16s}: ch {ones}")
        else:
            print(f"    [{r}] {name:16s}: (all off)")


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
