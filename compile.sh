#!/bin/bash

cd ~/.xmonad
rm xmonad.hi xmonad.o xmonad-x86_64-linux

path=$(dirname `readlink -f ./xmonad.hs`)
cd ${path}/xmonad-myconfig

cabal v1-update
cabal v1-install --user .
