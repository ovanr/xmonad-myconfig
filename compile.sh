#!/bin/bash

path=$(dirname `readlink -f ~/.xmonad/xmonad.hs`)
cd $path

cd xmonad-myconfig
cabal v1-install --user .
