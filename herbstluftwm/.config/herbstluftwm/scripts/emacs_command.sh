#!/usr/bin/env bash
cur_dir=$(dirname "$0")
if [ \"$(herbstclient get_attr clients.focus.class)\" != \"Emacs\" ]
then
    bash $cur_dir/switch_by_classes.sh 1 'Emacs' '' 'emacsclient -c'
    sleep 0.2
fi
bash $cur_dir/emacs_or_herbstluftwm_command.sh "$1"
