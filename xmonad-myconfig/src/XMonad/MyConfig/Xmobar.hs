{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig.Xmobar where

import XMonad 
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

myBar = "myXmobar"

fixLocation :: [String] -> [String]
fixLocation [w,l,t] =
   [ l, t, w ++ wChunk]
   where
      wChunk = replicate (30 - length w) ' '

fixLocation x = x

fixTitle :: String -> String
fixTitle str = xmobarColor "#ffffff"  "" $ take 60 $ tChunk ++ t ++ tChunk
   where
      t = shorten 50 str
      tChunk = flip replicate ' ' $ (61 - length t) `div` 2

fixLayout :: String -> String
fixLayout l = lChunk ++ l
   where
      lChunk = replicate (10 - length l) ' '

-- use xmobarPP config tos
myPP = xmobarPP { 
      ppCurrent = xmobarColor "#dAA520" "",
      ppTitle =  fixTitle, 
      ppLayout = fixLayout,
      ppSep = "",
      ppOrder = fixLocation
   }

toggleStrutsKey XConfig{ XMonad.modMask = modMask } = (modMask, xK_b)

withStatusBar :: LayoutClass l Window
              => XConfig l
              -> IO (XConfig (ModifiedLayout AvoidStruts l))
withStatusBar = statusBar myBar myPP toggleStrutsKey
