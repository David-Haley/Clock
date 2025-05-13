#!/bin/bash
# script to release IOT_Clock GPIO pins
echo 17 >/sys/class/gpio/unexport
# GEN0 VPRG
echo 18 >/sys/class/gpio/unexport
# GEN1 XLATE
echo 27 >/sys/class/gpio/unexport
# GEN2 Blank
echo 22 >/sys/class/gpio/unexport
# GEN3 XERROR
echo 23 >/sys/class/gpio/unexport
# GEN4 Comparitor
# echo 24 >/sys/class/gpio/unexport
# GEN5 not used
#echo 25 >/sys/class/gpio/unexport
# GEN6 not used
