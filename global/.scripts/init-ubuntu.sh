#!/bin/sh
sudo apt install stow

for script in $(find ~/.scripts/*/ -name init-ubuntu.sh) ; do sh $script ; done
