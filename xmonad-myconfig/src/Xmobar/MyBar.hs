{-# LANGUAGE FlexibleContexts #-}

module Xmobar.MyBar where

import Xmobar
import XMonad.MyConfig.Defaults (normalColor)

runXmobar = configFromArgs config >>= xmobar 


config = defaultConfig { 
   -- appearance
     font =   "xft:DejaVu Sans Mono-12"
   -- , bgColor =      "#22242b"
   -- , fgColor =      "#CACAC0"
   , bgColor =      "#000000"
   , fgColor =      "#ffffff"
   , position =     Top
   , border =       BottomB
   , borderColor =  "#000000"
   , borderWidth = 3
   , alpha = 255
   , additionalFonts = [ "xft:Symbola-9"
                       , "xft:Symbola-10"
                       , "xft:Symbola-11"
                       , "xft:Symbola-11"
                       , "xft:DejaVu Sans Mono-9"
                       , "xft:FontAwesome-10"]
   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "{}"  -- separator between left-right alignment
   , template = "%battery%  %multicpu% %multicoretemp%  %memory%  %dynnetwork% { %StdinReader% }  %kbd%  %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       False    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands = 

        -- weather monitor
        [ Run $ Weather "17607" [ "--template", " | <fc=#4682B4><tempC></fc>°C" ] 36000

        -- network activity monitor (dynamic interface resolution)
        , Run $ DynNetwork [ "--template" , "<action=`gnome-terminal -- nethogs`>↑ <tx> ↓ <rx></action>"
                           , "--Low"      , "524288"      -- units: B/s
                           , "--High"     , "10485760"    -- units: B/s
                           , "--low"      , normalColor
                           , "--normal"   , "#dAA520"
                           , "--high"     , "red"
                           ] 20

        -- cpu activity monitor
        , Run $ MultiCpu [ "--template" , "<action=`gnome-terminal -- top -o +%CPU`>▣ <total>%</action>"
                         , "--Low"      , "15"       -- units: %
                         , "--High"     , "60"       -- units: %
                         , "--low"      , normalColor
                         , "--normal"   , "#dAA520"
                         , "--high"     , "red"
                         ] 20

        -- cpu core temperature monitor
        , Run $ MultiCoreTemp  [ "--template" , "<action=`sudo fx --toggle-fan`><max>°</action>"
                               , "--Low"      , "60"        -- units: °C
                               , "--High"     , "80"        -- units: °C
                               , "--low"      , normalColor
                               , "--normal"   , "#dAA520"
                               , "--high"     , "red"
                               ] 50
                          
        -- memory usage monitor
        , Run $ Memory [ "--template" ,"<action=`gnome-terminal -- top -o +%MEM`>ℝ <usedratio>%</action>"
                       , "--Low"      , "30"        -- units: %
                       , "--High"     , "80"        -- units: %
                       , "--low"      , normalColor
                       , "--normal"   , "#dAA520"
                       , "--high"     , "red"
                       ] 50

        -- battery monitor
        , Run $ Battery [ "--template" , "<action=`xfce4-power-manager-settings -d battery_BAT1`><acstatus></action>"
                        , "--Low"      , "10"        -- units: %
                        , "--High"     , "60"        -- units: %
                        , "--low"      , "red"
                        , "--normal"   , "#dAA520"
                        , "--high"     , normalColor

                        , "--" -- battery specific options
                                  -- discharging status
                                  , "-o"  , "<fc=#dAA520>↓ <left></fc>"
                                  -- AC "on" status
                                  , "-O"  , "<fc=" ++ normalColor ++ ">↑ <left></fc>"
                                  -- charged status
                                  , "-i"  , "<fc=" ++ normalColor ++ ">↯ <left></fc>"
                                  -- , "-a", "notify-send -u critical 'Battery is running out!'"
                                  -- , "-A", "10"
                        ] 50

        , Run StdinReader
        
        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date "<action=`gnome-calendar`>%d-%b %T</action>" "date" 10

        -- keyboard layout indicator
        , Run $ Kbd [("us"         , "<fc=" ++ normalColor ++">en</fc>")
                    ,("gr"         , "<fc=#dAA520>ελ</fc>")
                    ]
        ]
   }
