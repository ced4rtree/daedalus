#!/usr/bin/env bash

xinput set-int-prop $(xinput list | grep 'Touchpad' | sed -e 's/.*id=//' -e 's/\s.*//') $(xinput list-props 14 | grep -i 'natural scrolling enabled (' | sed -e 's/.*(//' -e 's/).*//') 8 1
