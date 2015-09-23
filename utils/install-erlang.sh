#!/bin/sh

export HOME="/var/lib/ebill"
cd $HOME
/usr/bin/curl -s -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
mkdir ~/bin

PATH=$PATH:~/bin/
mv kerl ~/bin
chmod +x ~/bin/kerl
kerl update releases
kerl build 17.5 17.5
kerl install 17.5 ~/.kerl/installs
