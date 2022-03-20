#!/usr/bin/env bash
mkdir -p /usr/bin

curl -s https://api.github.com/repos/kmonad/kmonad/releases \
  | grep "browser_download_url.*-linux" \
  | sed -e 's/^ *//g' -e 's/"//g' \
  | cut -d' ' -f2 \
  | sort -rf \
  | head -1 \
  | xargs wget -O /usr/bin/kmonad

chmod +x /usr/bin/kmonad
