#!/usr/bin/env bash
selected_option=$(echo -e "shutdown\nreboot\nlogout" | rofi -dmenu -i -p "Power -  ")

if [ $? -eq 0 ]
then
    case $selected_option in
        'shutdown')
            shutdown -h 0
        ;;
        'reboot')
            reboot
        ;;
        'logout')
            gnome-session-quit --force
        ;;
    esac
fi
