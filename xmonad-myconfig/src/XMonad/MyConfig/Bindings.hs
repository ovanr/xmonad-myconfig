{-# LANGUAGE TupleSections #-}

module XMonad.MyConfig.Bindings where

import XMonad
import XMonad.Actions.GroupNavigation
import XMonad.MyConfig.Defaults
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import Text.Read (readMaybe)
import Data.Maybe (isJust)

------------------------------------------------------------------------
-- Key bindings

normalise :: [([(KeyMask, KeySym)], X ())] -> [((KeyMask, KeySym), X ())]
normalise = concatMap $ \(ks,x) -> map (,x) ks

myKeys conf@XConfig{ XMonad.modMask = modm } = M.fromList . normalise $

    [ 

    -- Application Shortcuts 
      ([(modm, xK_Return)], spawn myTerminal)
    , ([(modm, xK_a)],      spawn myAppFinder)
    , ([(modm, xK_w)],      spawn myWebBrowser)
    , ([(modm, xK_f)],      spawn myFileManager)
    , ([(modm, xK_e)],      spawn myEmailClient)
    , ([(modm, xK_d)],      spawn "discord" >> windows (W.greedyView "discord"))
    , ([(modm, xK_z)],      spawn "zoom" >> windows (W.greedyView "zoom"))
    , ([(modm, xK_s)],      spawn "skypeforlinux" >> windows (W.greedyView "skype"))

    -- Window Management Shortcuts
    
    -- close focused window
    , ([(modm .|. shiftMask, xK_c)], kill)

     -- Rotate through the available layout algorithms
    , ([(modm, xK_space)], sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ([(modm .|. shiftMask, xK_space)], setLayout $ XMonad.layoutHook conf)

    -- Move focus to the next window
    , ([(modm, xK_j)], windows W.focusDown)

    -- Move focus to the previous window
    , ([(modm, xK_k)], windows W.focusUp)

    -- Move focus to the master window
    , ([(modm, xK_m)], windows W.focusMaster)

    -- Swap the focused window and the master window
    , ([(modm .|. shiftMask, xK_m)], windows W.swapMaster)

    -- Swap the focused window with the next window
    , ([(modm .|. shiftMask, xK_j)], windows W.swapDown)

    -- Swap the focused window with the previous window
    , ([(modm .|. shiftMask, xK_k)], windows W.swapUp)

    -- Shrink the master area
    , ([(modm, xK_h)], sendMessage Shrink)

    -- Expand the main area
    , ([(modm, xK_l)], sendMessage Expand)

    -- Push window back into tiling
    , ([(modm, xK_t)], withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ([(modm, xK_comma)], sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ([(modm, xK_period)], sendMessage (IncMasterN (-1)))

    -- Take a screenshot
    , ([(modm, xK_Print)], spawn myScreenshooter)
    
    ---- Mute audio 
    , ([(noModMask, xF86XK_AudioMute)], 
         spawn "amixer -D pulse sset Master 0%")
    
    ---- Increase the volume
    , ([(noModMask, xF86XK_AudioRaiseVolume)], 
         spawn "amixer -D pulse sset Master 5%+")
    --
    ---- Decrease the volume
    , ([(noModMask, xF86XK_AudioLowerVolume)], 
         spawn "amixer -D pulse sset Master 5%-")

    -- Increase the brightness 
    , ([(noModMask, xF86XK_MonBrightnessUp)], spawn "xbacklight -inc 5")
     
    -- Decrease the brightness 
    , ([(noModMask, xF86XK_MonBrightnessDown)], spawn "xbacklight -dec 5")

    -- Turn off screen 
    , ([(noModMask, xF86XK_ScreenSaver)], spawn "xset dpms force off")
    
    -- Session Management Shortcuts
    
    -- Lock Screen 
    , ([(modm .|. shiftMask, xK_l)], spawn (myLocker !! 1))
    
    -- quit xmonad
    , ([(modm .|. shiftMask, xK_q)], liftIO exitSuccess)

    -- Restart xmonad
    , ([(modm .|. shiftMask, xK_r)], 
         spawn $ "killall nm-applet;" ++
                 "killall wallpaper_rnd_indicator;" ++
                 "killall caffeine;" ++
                 "killall pasystray;" ++
                 "killall redshift;" ++
                 "killall stalonetray;" ++
                 "${HOME}/.xmonad/./compile.sh; xmonad --restart")
    ] 
    ++
    -- Workspace Management Shortcuts
    
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [([(m .|. modm, k)], windows $ f i)
        | (i, k) <- zip (filter strIsInt . XMonad.workspaces  $ conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ([(modm, xK_quoteleft) ], focusLastWorkspace) ]
    ++

    [([(shiftMask .|. modm, xK_e)], windows $ W.shift "email"),
     ([(shiftMask .|. modm, xK_d)], windows $ W.shift "discord"),
     ([(shiftMask .|. modm, xK_z)], windows $ W.shift "zoom"),
     ([(shiftMask .|. modm, xK_s)], windows $ W.shift "skype")]

strIsInt :: String -> Bool
strIsInt = isJust . (readMaybe :: String -> Maybe Int)

focusLastWorkspace = do
   currentWorkspace <- W.workspace . W.current <$> withWindowSet return
   nextMatch History (query currentWorkspace)
   where
      isWindowInWorkspace workspace window =
         maybe False 
               (\s -> window == W.focus s || elem window (W.up s) || elem window (W.down s))
               (W.stack workspace)
      query currentWorkspace =
         not . isWindowInWorkspace currentWorkspace <$> ask

------------------------------------------------------------------------
-- Mouse bindings

myMouseBindings XConfig{ XMonad.modMask = modm } = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel 
    -- (button4 and button5)
    ]
