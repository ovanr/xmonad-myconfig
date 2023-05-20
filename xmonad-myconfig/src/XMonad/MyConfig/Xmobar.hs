{-# LANGUAGE FlexibleContexts #-}
module XMonad.MyConfig.Xmobar where

import XMonad 
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

myBar = "myXmobar"

myOrder :: [String] -> [String]
myOrder (w:l:t:_) = [w, t]

myTitle :: String -> String
myTitle str = take 200 $ "     " ++ (shorten 80 str) ++ repeat ' '

-- use xmobarPP config tos
myPP = xmobarPP { 
      ppCurrent = xmobarColor "#dAA520" "",
      ppTitle =  myTitle, 
      ppLayout = id,
      ppSep = "",
      ppOrder = myOrder
   }

toggleStrutsKey XConfig{ XMonad.modMask = modMask } = (modMask, xK_b)

withStatusBar :: LayoutClass l Window
              => XConfig l
              -> IO (XConfig (ModifiedLayout AvoidStruts l))
withStatusBar = statusBar myBar myPP toggleStrutsKey
