{-# LANGUAGE FlexibleContexts #-}

module Xmobar.MyBar where

import Xmobar

runXmobar = configFromArgs config >>= xmobar 

config = defaultConfig { 
   -- appearance
     font =   "xft:DejaVu Sans Mono-12"
   , bgColor =      "#22242b"
   , fgColor =      "#CACAC0"
   , position =     Top
   , border =       BottomB
   , borderColor =  "black"
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
   , template = "%battery% | %multicpu% %multicoretemp% | %memory% | %dynnetwork% { %StdinReader% }  | %date% | %kbd% "

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
                           , "--Low"      , "10000"       -- units: B/s
                           , "--High"     , "10485760"    -- units: B/s
                           , "--low"      , "#7289da"
                           , "--normal"   , "darkorange"
                           , "--high"     , "darkred"
                           ] 10

        -- cpu activity monitor
        , Run $ MultiCpu [ "--template" , "<action=`gnome-terminal -- top -o +%CPU`>cpu: <total>%</action>"
                         , "--Low"      , "10"         -- units: %
                         , "--High"     , "1200"       -- units: %
                         , "--low"      , "#7289da"
                         , "--normal"   , "darkorange"
                         , "--high"     , "darkred"
                         ] 10

        -- cpu core temperature monitor
        , Run $ MultiCoreTemp  [ "--template" , "<action=`sudo fx --toggle-fan`> <max>°</action>"
                               , "--Low"      , "60"        -- units: °C
                               , "--High"     , "80"        -- units: °C
                               , "--low"      , "#7289da"
                               , "--normal"   , "darkorange"
                               , "--high"     , "darkred"
                               ] 50
                          
        -- memory usage monitor
        , Run $ Memory [ "--template" ,"<action=`gnome-terminal -- top -o +%MEM`>mem: <usedratio>%</action>"
                       , "--Low"      , "30"        -- units: %
                       , "--High"     , "90"        -- units: %
                       , "--low"      , "#7289da"
                       , "--normal"   , "darkorange"
                       , "--high"     , "darkred"
                       ] 10

        -- battery monitor
        , Run $ Battery [ "--template" , "<action=`xfce4-power-manager-settings -d battery_BAT1`><acstatus></action>"
                        , "--Low"      , "10"        -- units: %
                        , "--High"     , "60"        -- units: %
                        , "--low"      , "darkred"
                        , "--normal"   , "darkorange"
                        , "--high"     , "#7289da"

                        , "--" -- battery specific options
                                  -- discharging status
                                  , "-o"  , "↓ <left>"
                                  -- AC "on" status
                                  , "-O"  , "<fc=#dAA520>↑ <left></fc>"
                                  -- charged status
                                  , "-i"  , "<fc=#006000><left></fc>"
                                  -- , "-a", "notify-send -u critical 'Battery is running out!'"
                                  -- , "-A", "10"
                        ] 50

        , Run StdinReader
        
        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date "<action=`gnome-calendar`>%d-%b (%a) %T</action>" "date" 10

        -- keyboard layout indicator
        , Run $ Kbd [("us"         , "<fc=#7289da>EN</fc>")
                    ,("gr"         , "<fc=#dAA520>GR</fc>")
                    ]
        ]
   }
