#!/bin/sh
mkdir -p ~/Tools
mkdir -p ~/.bin/emacs
cd ~/Tools
git clone git://git.savannah.gnu.org/emacs.git -b feature/native-comp
cd emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-native-compilation --with-mailutils
make -j 8 NATIVE_FULL_AOT=1
sudo make install
