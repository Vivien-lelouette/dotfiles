#!/usr/bin/env bash
uname -a| grep Ubuntu
if [[ $(echo $?) -eq 0 ]]
then
   sudo apt install fonts-source-code-pro-ttf
   sudo mkfontscale /usr/share/fonts/truetype/source-code-pro-ttf
   sudo mkfontdir /usr/share/fonts/truetype/source-code-pro-ttf
   xset fp+ "/usr/share/fonts/truetype/source-code-pro-ttf"
fi
