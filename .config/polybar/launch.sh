#!/usr/bin/env bash

killall -q polybar

echo "---" | tee -a /tmp/polybar1.log
/home/some-guy/Downloads/polybar-dwm-module/build/bin/polybar main 2>&1 | tee -a /tmp/polybar1.log & disown

echo "Bars launched..."

# frankenlaunch
export brightdisp=$(xrandr | awk 'NR==2 {print $1}')
