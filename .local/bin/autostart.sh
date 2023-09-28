#!/bin/sh

mpv /opt/sounds/startup-01.mp3 &
if [ -n "$(pidof Xorg)" ]; then
	xsetroot -cursor_name left_ptr
	setxkbmap -option ctrl:nocaps
	xset r rate 200 65
	xcompmgr &
	if [ "$1" != "--no-polybar" ]; then ~/.config/polybar/launch.sh &; fi
else
	eww open bar0
fi

wallpaper.sh
natScroll.sh
nm-applet &
batsignal -M 'dunstify' &
if [ -z "$(pidof mpd)" ]; then mpd; fi # sometimes mpd gets started multiple times, and it sounds like hot garbage getting spoon fed directly into my ears

# make java apps work
export _JAVA_AWT_WM_NONREPARENTING=1
wmname "LG3D"
