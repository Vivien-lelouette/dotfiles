#!/bin/sh
mkdir -p ~/Tools
mkdir -p ~/.bin/emacs
cd ~/Tools
git clone git://git.savannah.gnu.org/emacs.git -b feature/native-comp
cd emacs
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --prefix=$(pwd ~)/.bin/emacs --without-all --without-x --with-zlib --with-nativecomp --disable-silent-rules --with-modules --with-file-notification=inotify --with-mailutils --with-lcms2 --with-imagemagick --with-json CFLAGS="-O3 -s -mtune=native -march=native -fomit-frame-pointer"
make -j 8 NATIVE_FULL_AOT=1
make install
