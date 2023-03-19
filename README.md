# IOT_Clock
Software for a clock based on purpose built hardware driven by a Raspberry Pi 3B.

## Hardware
 The hardware provides two six digit seven segment displays (0.8" high) and sixty four individual LEDs (5mm) to provide a simulated sweephand with reference markers at 0, 15 ,30 and 45.
 The hardware suports automatic brightness control over a 4095 to one range with 64 levels of analogue brightness compensation to match the brightness of indicidual LEDs and segments.
 The clock can chime by playing arbitary .wav files (uses amixer and aplayer).

## Other Libraries
Uses Events_and_Errors from DJH repository, SPI, GPIO and TLC5940 from Pi_Common repository. The SPI interface from Pi_Common_C repository is used.
