#!/usr/bin/env bash

device=$(xinput list | grep 'Touchpad' | sed -e 's/.*id=//' -e 's/\s.*//')
xinput set-int-prop $device $(xinput list-props $device | grep -i 'natural scrolling enabled (' | sed -e 's/.*(//' -e 's/).*//') 8 1
