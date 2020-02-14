#!/bin/bash

[ -f ~/.xinitrc ] && mv ~/.xinitrc ~/.xinitrc.bak
ln -s ~/rainy/xinitrc ~/.xinitrc

[ -f ~/.bashrc ] && mv ~/.bashrc ~/.bashrc.bak
ln -s ~/rainy/bashrc ~/.bashrc

[ -f ~/.inputrc ] && mv ~/.inputrc ~/.inputrc.bak
ln -s ~/rainy/inputrc ~/.inputrc

[ -f ~/.xmonad ] && mv ~/.xmonad ~/.xmonad.bak
ln -s ~/rainy/xmonad ~/.xmonad
