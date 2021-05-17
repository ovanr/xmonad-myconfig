#!/bin/bash

path=$(dirname `readlink -f ~/.xmonad/xmonad.hs`)
cd $path/xmonad-myconfig
cabal v1-install --user .
