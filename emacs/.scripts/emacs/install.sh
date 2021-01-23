#!/bin/sh
mkdir -p ~/Tools
cd ~/Tools
git clone -b master git://git.sv.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure --with-modules
make
sudo make install
