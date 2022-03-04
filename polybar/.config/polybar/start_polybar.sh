#!/usr/bin/env bash
pkill polybar
for m in $(polybar --list-monitors | sed 's/ //g'); do
    curr_monitor=$(echo "$m" | cut -d":" -f1)
    max_title_width=$(($[$(echo "$m" | cut -d":" -f2 | cut -d"x" -f1)-800] / 9))
    MONITOR=$curr_monitor MAX_TITLE_WIDTH=$max_title_width polybar --quiet --reload hlwm &
done
