# xmonad-myconfig

My xmonad-xmobar-stalonetrayrc config

Xmonad and Xmobar configs are nicely packaged

## Installation (Arch)

1. Install needed packages
```
sudo pacman -S xmonad xmonad-contrib xmobar stalonetray 
```

2. Install extra dependencies
```
sudo pacman -S network-manager-applet pasystray xfce4-power-manager redshift xorg-backlight xorg-xset alsa-utils xfce4-appfinder xfce4-screensaver picom
yay -S caffeine-ng
```

3. Link stalonetrayrc and xmonad.hs to .xmonad dir
```
ln -s `pwd`/stalonetrayrc ${HOME}/.xmonad/stalonetrayrc
ln -s `pwd`/xmonad.hs ${HOME}/.xmonad/xmonad.hs
```

4. Compile xmonad-myconfig to global scope

```
cd xmonad-myconfig
sudo cabal v1-install --global .
```

