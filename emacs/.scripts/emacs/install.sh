mkdir -p ~/Tools/emacs
cd ~/Tools
git clone -b master git://git.sv.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure --with-modules
make
sudo make install
