#!/bin/sh
mkdir -p ~/Tools
cd ~/Tools
git clone git://git.savannah.gnu.org/emacs.git -b feature/native-comp
cd emacs
./autogen.sh
./configure --with-nativecomp --with-modules --with-xwidgets --with-cairo --with-mailutils --with-jansson
make -j 8
