{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- Xmonad config file.

module XMonad.MyConfig.Config where

import XMonad
import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.DynamicLog
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import Control.Monad
import XMonad.MyConfig.Defaults
import XMonad.MyConfig.Bindings
import XMonad.MyConfig.Layout
import XMonad.MyConfig.StartupHooks (myStartupHook)
import XMonad.MyConfig.Xmobar (withStatusBar)

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
myWorkspaces    = map show [1..5] ++ [ "email", "discord", "zoom", "skype" ] 

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#50504c"
myFocusedBorderColor = "#00CD00"


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
    [ className =? "Thunderbird" --> doShift "email"
    , className =? "discord"     --> doShift "discord"
    , className =? "Zoom"        --> doShift "zoom"
    , className =? "Skype"       --> doShift "skype"
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_ABOVE"  --> doFloat
    , stringProperty "_NET_WM_STATE(ATOM)" =? "_NET_WM_STATE_STAYS_ON_TOP"  --> doFloat
    ]

------------------------------------------------------------------------
-- Event handling

-- EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = ewmhDesktopsEventHook 

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = ewmhDesktopsLogHook <+> 
            fadeInactiveLogHook transparency <+>
            historyHook
            
   where
      transparency = 0xdeeeeeeeee -- lower values -> more transparent



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
        manageHook         = manageDocks <+> myManageHook <+> manageHook def,
        handleEventHook    = myEventHook,
        logHook            = myLogHook, 
        startupHook        = myStartupHook
    }

------------------------------------------------------------------------
-- Main

main :: IO ()
main = withStatusBar myConfig >>= xmonad

