#!/usr/bin/env bash
selected_option=$(gnome-control-center -l | sed -e '1d' -e 's/^[[:space:]]*//' | rofi -dmenu -i -p "")

if [ $? -eq 0 ]
then
    gnome-control-center $selected_option
fi
