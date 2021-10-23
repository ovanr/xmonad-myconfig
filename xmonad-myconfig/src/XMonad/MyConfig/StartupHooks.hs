module XMonad.MyConfig.StartupHooks where

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Cursor
import XMonad.MyConfig.Defaults

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do
   ewmhDesktopsStartup 
   
   -- set mouse cursor 
   setDefaultCursor xC_left_ptr
   
   -- set keyboard layout toggle
   spawn "setxkbmap -option grp:rctrl_rshift_toggle us,gr"

   -- enable tap to click
   let touchpad = " 'ELAN1200:00 04F3:3090 Touchpad' "
       tapOpt = " 'libinput Tapping Enabled' "
   spawn $ "xinput set-prop " ++ touchpad ++ tapOpt ++ "1"
   
   -- listen for notify events 
   --spawn "/usr/lib/notification-daemon-1.0/notification-daemon"
  
   -- spawn power management 
   spawn "xfce4-power-manager"
   
   -- set wallpaper
   spawn "${HOME}/./.fehbg"
   
   -- compositor
   spawn "picom"

   -- tray applets
   spawn $ "stalonetray -c " ++ stalonetrayConfig
   spawn "pasystray"
   spawn "nm-applet"
   spawn "caffeine"
   spawn "wallpaper_rnd_indicator"

   -- night light switcher
   spawn "redshift -x; redshift -l 35.1753:33.3642 -t 6500:3500" 
   
   -- spawn auto-locking program
   spawn $ head myLocker

   -- needed for Java Swing GUI windows to work
   setWMName "LG3D"
