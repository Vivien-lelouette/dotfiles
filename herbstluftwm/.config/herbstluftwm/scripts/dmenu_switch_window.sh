#!/usr/bin/env bash
export FG_COLOR='#d8dee9'
export FG_ALT_COLOR='#93979f'
export BG_COLOR='#2e3440'
export BG_ALT_COLOR='#242832'
export FOCUS_COLOR='#a3be8c'
export ALERT_COLOR='#b48ead'

cur_dir=$(dirname "$0")
client_list=$(bash $cur_dir/list_switchable_clients.sh 1)

display_client_list=$(echo "${client_list}" \
                          | while read -r winid other;do \
                          echo \
                              $winid \
                              $(herbstclient get_attr clients.$winid.class): \
                              $(herbstclient get_attr clients.$winid.title) \
                          ;done \
                          | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//')

client=$(echo "$display_client_list" | cut -d' ' -f2- | dmenu -i -p 'Windows' -fn 'Dejavu Sans:pixelsize=11' -nb "$BG_ALT_COLOR" -nf "$FG_COLOR" -sb "$FOCUS_COLOR" -sf "$BG_ALT_COLOR")

if [ $? -eq 0 ]
then
    clientid=$(echo "${display_client_list}" | grep " $client" | head -1 | cut -d' ' -f1)
    echo $clientid
    `$(echo "${client_list}" | grep "^$clientid" | sed "s/.*\ \"/\"/g" | xargs echo)`
fi
