#!/usr/bin/env bash
if [ "$(herbstclient get_attr clients.focus.class)" == "Emacs" ]
then
    timeout 0.2 bash -c "emacsclient -e \"(with-current-buffer (window-buffer (selected-window)) $1)\""
    if [ $? -ne 0 ]
    then
        bash -c "$2"
    fi
else
    bash -c "$2"
fi
