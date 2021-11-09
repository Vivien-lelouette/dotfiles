#!/usr/bin/env bash
  if [ "$(herbstclient get_attr clients.focus.class)" == "Emacs" ]
  then
      timeout 0.2 emacsclient -e "(windmove-$1)"
      if [ $? -ne 0 ]
      then
          herbstclient focus $1
      fi
  else
      herbstclient focus $1
  fi
