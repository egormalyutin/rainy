#!/bin/bash

devmon &
start-pulseaudio-x11 &
export _JAVA_AWT_WM_NONREPARENTING=1

# exec spectrwm
exec xmonad
