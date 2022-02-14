#!/usr/bin/env bash
  script_folder="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
  if [ \"$(herbstclient get_attr clients.focus.class)\" != \"Emacs\" ]
  then
      client_list=$(bash $script_folder/list_switchable_clients.sh 0 0)
      emacs_client_id=$(echo "${client_list}" \
                            | sed -e 's/^ //g' -e 's/^[ \t]*//;s/[ \t]*$//' \
                            | grep " Emacs \"" \
                            | tail -1 \
                            | sed 's/¤.*//g')
      echo $emacs_client_id
      if [ -z "$emacs_client_id" ]
      then
        emacsclient -c &
      else
        herbstclient bring $emacs_client_id &
      fi
      sleep 0.5
  fi
  bash $script_folder/emacs_or_herbstluftwm_command.sh "$1"
