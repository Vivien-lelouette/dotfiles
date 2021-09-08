#!/usr/bin/env bash
script_folder="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
if [ \"$(herbstclient get_attr clients.focus.class)\" != \"Emacs\" ]
then
    bash $script_folder/switch_by_classes.sh 1 'Emacs' '' 'emacsclient -c'
    sleep 0.2
fi
bash $script_folder/emacs_or_herbstluftwm_command.sh "$1"
