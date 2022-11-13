#!/usr/bin/env bash
mkdir -p ~/Tools
cd ~/Tools
git clone --branch emacs-28 git://git.savannah.gnu.org/emacs.git
cd emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
v./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg --with-x-toolkit=gtk --with-xwidgets --with-imagemagick --with-cairo --with-mailutils --with-gnutls -without-pop --with-json --with-native-compilation
make -j 8 NATIVE_FULL_AOT=1
sudo make install
