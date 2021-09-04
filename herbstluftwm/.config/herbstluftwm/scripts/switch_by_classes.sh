#!/usr/bin/env bash
direction=$1
include_classes=$2
exclude_classes=$3
fallback_command=$4

init_state="$(herbstclient dump)"

cur_dir=$(dirname "$0")

switchable_clients=$(bash $cur_dir/list_switchable_clients.sh | sed '/^$/d')

if [ -n "$include_classes" ]
then
    switchable_clients=$(echo "$switchable_clients" | grep "$include_classes")
fi

if [ -n "$exclude_classes" ]
then
    switchable_clients=$(echo "$switchable_clients" | grep -v "$exclude_classes")
fi

if [ $direction -lt 0 ]
then
    switchable_clients=$(echo "$switchable_clients" | tail -2 | head -1)
else
    switchable_clients=$(echo "$switchable_clients" | head -1)
fi

`$(echo $switchable_clients | sed "s/.*\ \"/\"/g" | xargs echo)`

if [ "$(herbstclient dump)" = "$init_state" ]
then
    if echo "$include_classes" | grep -qE "$(herbstclient get_attr clients.focus.class)"
    then
        if [ "$(herbstclient get_attr clients.focus.class)" == "" ]
        then
            nohup `$($fallback_command)` &
            sleep 0.1
        fi
    else
        nohup `$($fallback_command)` &
        sleep 0.1
    fi
fi
