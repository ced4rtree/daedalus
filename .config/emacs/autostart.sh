#!/usr/sh
xset r rate 200 65
setxkbmap -option ctrl:nocaps

pipewire &
pipewire-pulse &
wireplumber &

picom --vsync &
