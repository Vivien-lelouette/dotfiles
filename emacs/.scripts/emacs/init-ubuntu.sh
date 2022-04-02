#!/bin/sh
sudo apt-get update \
    && apt-get install -y \
               apt-transport-https \
               ca-certificates \
               curl \
               gnupg-agent \
               software-properties-common

sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa \
    && apt-get update -y \
    && apt-get install -y gcc-10 libgccjit0 libgccjit-10-dev

sudo apt-get install -y libjansson4 libjansson-dev git

sudo apt-get install autoconf make gcc texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev libncurses5-dev

sudo apt install cmake libtool libtool-bin zsh
