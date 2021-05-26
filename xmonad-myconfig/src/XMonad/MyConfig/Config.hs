{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- Xmonad config file.

module XMonad.MyConfig.Config where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Cursor
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import Control.Monad
import XMonad.MyConfig.Defaults
import XMonad.MyConfig.Bindings

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False 

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth :: Dimension
myBorderWidth = 2 

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask       = mod1Mask
myModMask2      = mod4Mask 

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
myWorkspaces    = map show [1..5] ++ [ "email" ] 

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#22242b"
myFocusedBorderColor = "#00CD00"


------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

type MyLayout = 
   Choose 
      (ModifiedLayout SmartBorder Tall) 
      (Choose 
         (Mirror (ModifiedLayout SmartBorder Tall))
         (Choose
            (ModifiedLayout SmartBorder Grid)
            (Choose
               (ModifiedLayout SmartBorder TwoPane)
               (ModifiedLayout WithBorder Full)
            )
         )
      )

myLayout :: MyLayout Window
myLayout = tiled ||| 
           Mirror tiled ||| 
           smartBorders Grid ||| 
           smartBorders twoPane ||| 
           noBorders Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = smartBorders $ Tall nmaster delta ratio
     twoPane = TwoPane delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Thunderbird" --> doShift "email" ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook 

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = ewmhDesktopsLogHook <+> fadeInactiveLogHook transparency
   where
      transparency = 0xdeeeeeeeee -- lower values -> more transparent

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
   -- spawn "batteryNotifier.sh"
   
   -- night light switcher
   spawn "redshift -x; redshift -l 35.1753:33.3642 -b 0.8:0.7 -t 6500:3500" 
   
   -- spawn auto-locking program
   spawn $ head myLocker

------------------------------------------------------------------------
-- Status bar

myBar = "myXmobar"

-- use xmobarPP config tos
myPP = xmobarPP { ppCurrent = xmobarColor "#7289da" "" . wrap "<" ">" }

toggleStrutsKey XConfig{ XMonad.modMask = modMask } = (modMask, xK_b)

withStatusBar :: LayoutClass l Window
              => XConfig l
              -> IO (XConfig (ModifiedLayout AvoidStruts l))
withStatusBar = statusBar myBar myPP toggleStrutsKey

stalonetrayConfig = "${HOME}/.xmonad/stalonetrayrc"

------------------------------------------------------------------------
-- The Config

-- defaults are defined in xmonad/XMonad/Config.hs
myConfig :: XConfig MyLayout
myConfig = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = manageDocks <+> myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook, 
        startupHook        = myStartupHook
    }

------------------------------------------------------------------------
-- Main

main :: IO ()
main = withStatusBar myConfig >>= xmonad

