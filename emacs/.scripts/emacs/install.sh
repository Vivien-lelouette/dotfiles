#!/bin/sh
mkdir -p ~/Tools
cd ~/Tools
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-x-toolkit=gtk --with-xwidgets  --with-cairo --with-mailutils -without-pop --with-native-compilation
make -j 8 NATIVE_FULL_AOT=1
sudo make install
