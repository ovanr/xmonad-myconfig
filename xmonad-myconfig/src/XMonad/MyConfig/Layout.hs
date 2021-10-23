module XMonad.MyConfig.Layout where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
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
type MyTiled = ModifiedLayout Rename (ModifiedLayout Spacing (ModifiedLayout SmartBorder Tall))
type MyMirrorTiled = ModifiedLayout Rename (Mirror MyTiled)
type MyGrid = ModifiedLayout Rename (ModifiedLayout Spacing (ModifiedLayout SmartBorder Grid))
type MyTwoPane = ModifiedLayout Rename (ModifiedLayout Spacing (ModifiedLayout SmartBorder TwoPane))
type MyFull = ModifiedLayout Rename (ModifiedLayout Spacing (ModifiedLayout SmartBorder Full))

type MyLayout = 
   Choose 
      MyTiled
      (Choose 
         MyMirrorTiled
         (Choose
            MyGrid
            (Choose
               MyTwoPane
               MyFull
            )
         )
      )

myLayout :: MyLayout Window
myLayout = tiled ||| 
           mirrorTiled ||| 
           grid ||| 
           twoPane ||| 
           full
  where
     border = Border 5 5 5 5
     withSpacing = spacingRaw True border False border True
     tiled   = renamed [Replace "V-Tiled"] $ withSpacing $ smartBorders $ Tall nmaster delta ratio
     mirrorTiled = renamed [Replace"H-Tiled"] $ Mirror tiled
     grid = renamed [Replace "Grid"] $ withSpacing $ smartBorders Grid
     twoPane = renamed [Replace "2-Pane"] $ withSpacing $ smartBorders $ TwoPane delta ratio
     full = renamed [Replace "Full"] $ withSpacing $ smartBorders Full
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

