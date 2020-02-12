#!/bin/bash

devmon &
start-pulseaudio-x11 &
export _JAVA_AWT_WM_NONREPARENTING=1
xrdb -load ~/rainy/xresources
/usr/local/bin/picom --config ~/rainy/compton.conf &
sleep 3 && xwinwrap -ni -fdt -sh rectangle -un -b -nf -ovr -fs -- mpv --wid WID --no-config --keepaspect=no --loop --no-border --vd-lavc-fast --x11-bypass-compositor=no --gapless-audio=yes --aid=no --vo=xv --hwdec=auto --really-quiet --pause --input-ipc-server=/tmp/mpv-bg-socket ~/rainy/bg.mp4 &
# { while true; do; if (($(acpi | sed 's/Char/100%/' | grep -Eo '[0-9]+%' | head -n1 | grep -Eo '[0-9]+') < 20)); then; zenity --warning --text='Battery low'; fi; sleep 20; done } &

exec spectrwm
