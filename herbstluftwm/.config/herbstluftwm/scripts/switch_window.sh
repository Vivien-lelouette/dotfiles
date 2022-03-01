#!/usr/bin/env bash
  script_folder="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
  client_list=$(bash $script_folder/list_switchable_clients.sh 1)

  display_client_list=$(echo "${client_list}" \
                            | grep -v " Emacs \"" \
                            | sed 's/¤//g' \
                            | while read -r winid other;do \
                            echo \
                                $winid¤\
                                $(herbstclient get_attr clients.$winid.class): \
                                $(herbstclient get_attr clients.$winid.title) \
                            ;done \
                            | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//')

  emacs_buffer_list=$(echo "${client_list}" \
                          | grep -e " Emacs \"" \
                          | sed 's/¤.*//g' \
                          | while read -r name;do \
                          echo \
                              $name¤\
                              "Emacs: " \
                              $name \
                          ;done)


  display_client_list=$(echo "$display_client_list \

  $emacs_buffer_list")

  client=$(echo "$display_client_list" | sed 's/.*¤ //g' | rofi -dmenu -i -p "")

  if [ $? -eq 0 ]
  then
      clientid=$(echo "${display_client_list}" | grep " $client" | head -1 | sed 's/¤.*//g')
      cmd=$(echo "${client_list}" | grep "^${clientid}¤" | sed 's/.* \"/\"/g')
      $(echo ${cmd:1:-1})
  fi
