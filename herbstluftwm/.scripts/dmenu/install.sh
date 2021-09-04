#!/usr/bin/env bash
mkdir -p ~/Tools
cd ~/Tools
git clone https://git.suckless.org/dmenu

cd dmenu
wget https://tools.suckless.org/dmenu/patches/mouse-support/dmenu-mousesupport-5.0.diff
wget https://tools.suckless.org/dmenu/patches/numbers/dmenu-numbers-4.9.diff

git apply dmenu-mousesupport-5.0.diff
git apply dmenu-numbers-4.9.diff

sudo make clean install
