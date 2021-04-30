#!/bin/sh
mkdir -p ~/Tools/emacs-pgtk-nativ-comp
# cd ~/Tools
# git clone git://git.savannah.gnu.org/emacs.git -b feature/native-comp
cd ~/Tools/emacs-pgtk-nativ-comp
cd emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg \
         --with-tiff --with-xft --with-xpm --with-gpm=no \
         --with-modules --with-native-compilation --with-pgtk --with-cairo
make -j 8 NATIVE_FULL_AOT=1
sudo make install
