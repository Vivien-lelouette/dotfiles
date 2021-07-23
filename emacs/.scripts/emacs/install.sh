#!/bin/sh
mkdir -p ~/Tools/emacs-pgtk-nativ-comp

cd ~/Tools
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
# Will add --with-pgtk when merged to master
./configure --with-dbus --with-gif --with-jpeg --with-png --with-rsvg \
 --with-tiff --with-xft --with-xpm --with-gpm=no \
 --with-modules --with-native-compilation --with-cairo --with-pgtk
make -j 8 NATIVE_FULL_AOT=1
sudo make install
