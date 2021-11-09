#!/usr/bin/env bash
  direction=$1
  include_classes=$2
  exclude_classes=$3
  fallback_command=$4

  init_state="$(herbstclient dump)"

  script_folder="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

  switchable_clients=$(bash $script_folder/list_switchable_clients.sh 0 1 | sed '/^$/d')

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

  cmd=$(echo $switchable_clients | sed "s/.*\ \"/\"/g")

  $(echo ${cmd:1:-1})

  if [ "$(herbstclient dump)" = "$init_state" ]
  then
      if echo "$include_classes" | grep -qE "$(herbstclient get_attr clients.focus.class)"
      then
          if [ "$(herbstclient get_attr clients.focus.class)" == "" ]
          then
              $(echo ${fallback_command})
              sleep 0.1
          fi
      else
          $(echo ${fallback_command})
          sleep 0.1
      fi
  fi
