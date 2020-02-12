#!/bin/bash

[ -f ~/.spectrwm.conf ] && mv ~/.spectrwm.conf ~/.spectrwm.conf.bak
ln -s ~/rainy/spectrwm.conf ~/.spectrwm.conf

[ -f ~/.xinitrc ] && mv ~/.xinitrc ~/.xinitrc.bak
ln -s ~/rainy/xinitrc ~/.xinitrc

[ -f ~/.bashrc ] && mv ~/.bashrc ~/.bashrc.bak
ln -s ~/rainy/bashrc ~/.bashrc

[ -f ~/.inputrc ] && mv ~/.inputrc ~/.inputrc.bak
ln -s ~/rainy/inputrc ~/.inputrc
