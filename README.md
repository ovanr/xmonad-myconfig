# xmonad-myconfig

My xmonad-xmobar-stalonetrayrc config

Xmonad and Xmobar configs are nicely packaged

## Preview

![preview](https://raw.githubusercontent.com/ovanr/xmonad-myconfig/main/screenshot.png)

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

3. Link stalonetrayrc, xmonad.hs and compile.sh to .xmonad dir
```
ln -s `pwd`/stalonetrayrc ${HOME}/.xmonad/stalonetrayrc
ln -s `pwd`/xmonad.hs ${HOME}/.xmonad/xmonad.hs
ln -s `pwd`/compile.sh ${HOME}/.xmonad/compile.sh
```

4. Enable pacman hook that recompiles config package when new 
   xmonad or xmobar packages are available
```
sudo cp xmonad-myconfig.hook /etc/pacman.d/hooks
```

5. Compile xmonad-myconfig

```
./compile.sh
```


