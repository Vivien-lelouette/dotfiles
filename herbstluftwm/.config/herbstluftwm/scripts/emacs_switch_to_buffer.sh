#!/usr/bin/env bash
  script_folder="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
  bash $script_folder/emacs_command.sh "(switch-to-buffer \\\"$(echo $@ | sed -e "s/'//g")\\\")"
