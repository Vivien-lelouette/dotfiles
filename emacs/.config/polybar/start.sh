#!/bin/sh
if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar --reload -c ~/.config/polybar/config.txt panel &
  done
else
  polybar --reload -c ~/.config/polybar/config.txt panel &
fi
