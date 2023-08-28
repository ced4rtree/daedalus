#!/bin/sh

mpv /opt/sounds/startup-01.mp3 &
xsetroot -cursor_name left_ptr
wallpaper.sh
natScroll.sh
xset r rate 200 65
setxkbmap -option caps:escape
picom &
~/.config/polybar/launch.sh
nm-applet &
batsignal -M 'dunstify' &
if [ -z "$(pidof mpd)" ]; then mpd; fi # sometimes mpd gets started multiple times, and it sounds like hot garbage getting spoon fed directly into my ears

# make java apps work
export _JAVA_AWT_WM_NONREPARENTING=1
wmname "LG3D"
