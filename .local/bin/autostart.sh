#!/bin/sh

mpv /opt/sounds/startup-01.mp3 &
xsetroot -cursor_name left_ptr
wallpaper.sh
natScroll.sh
xset r rate 200 65
setxkbmap -option ctrl:nocaps
xcompmgr &
if [ "$1" != "--no-polybar" ]; then ~/.config/polybar/launch.sh &; fi
nm-applet &
batsignal -M 'dunstify' &
if [ -z "$(pidof mpd)" ]; then mpd; fi # sometimes mpd gets started multiple times, and it sounds like hot garbage getting spoon fed directly into my ears

# make java apps work
export _JAVA_AWT_WM_NONREPARENTING=1
wmname "LG3D"
