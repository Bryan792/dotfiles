import XMonad
-- LAYOUTS
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen 
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
-- WINDOW RULES
import XMonad.ManageHook
-- KEYBOARD & MOUSE CONFIG
import XMonad.Util.EZConfig
import XMonad.Actions.FloatKeys
import Graphics.X11.ExtraTypes.XF86
-- STATUS BAR
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Dmenu
--import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import System.IO (hPutStrLn)
--import XMonad.Operations
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS     -- nextWS, prevWS
import Data.List      -- clickable workspaces
import Data.Ratio ((%))   -- clickable workspaces


--------------------------------------------------------------------------------------------------------------------
-- DECLARE WORKSPACES RULES
--------------------------------------------------------------------------------------------------------------------
myLayout = onWorkspace (myWorkspaces !! 0) (avoidStruts (tiledSpace ||| tiled) ||| fullTile)
    $ onWorkspace (myWorkspaces !! 1) (avoidStruts (noBorders(tiledSpace ||| fullTile)) ||| fullScreen)
    $ onWorkspace (myWorkspaces !! 2) (avoidStruts simplestFloat)
    $ onWorkspace (myWorkspaces !! 6) (avoidStruts pidginLayout)
--    --$ onWorkspace  (myWorkspaces !! 6) (pidginLayout)
    $ avoidStruts ( tiledSpace  ||| tiled ||| fullTile ) 
  where
    tiled     = spacing 5 $ ResizableTall nmaster delta ratio [] 
    tiledSpace    = spacing 60 $ ResizableTall nmaster delta ratio [] 
    fullScreen  = noBorders(fullscreenFull Full)
    fullTile  = ResizableTall nmaster delta ratio [] 
    fullTiled   = ResizableTall nmaster delta ratio [] 
    borderlessTile  = noBorders(fullTile)
    -- Default number of windows in master pane
    nmaster = 1
    -- Percent of the screen to increment when resizing
    delta   = 5/100
    -- Default proportion of the screen taken up by main pane
    ratio   = toRational (2/(1 + sqrt 5 :: Double))
    gridLayout = spacing 8 $ Grid
    pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
    pidginLayout = withIM (1%9) pidginRoster gridLayout

--------------------------------------------------------------------------------------------------------------------
-- WORKSPACE DEFINITIONS
--------------------------------------------------------------------------------------------------------------------
myWorkspaces = clickable $ ["term"
    ,"web"  
    ,"float"  
    ,"docs" 
    ,"tunes"
    ,"mail" 
    ,"pidgin"]  

  where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
            (i,ws) <- zip [1..] l,
            let n = i ]
      
--------------------------------------------------------------------------------------------------------------------
-- APPLICATION SPECIFIC RULES
--------------------------------------------------------------------------------------------------------------------
myManageHook = composeAll   [ resource =? "dmenu" --> doFloat
        , resource =? "skype"   --> doFloat
        , resource =? "mplayer" --> doFloat
        , resource =? "feh" --> doFloat
        , resource =? "google-chrome"--> doShift (myWorkspaces !! 1)
        , className =? "Pidgin"--> doShift (myWorkspaces !! 6)
        , resource =? "lowriter"--> doShift (myWorkspaces !! 3)
        , resource =? "localc"--> doShift (myWorkspaces !! 3)
        , resource =? "loimpress"--> doShift (myWorkspaces !! 3)
        , resource =? "zathura"--> doShift (myWorkspaces !! 3)
        , resource =? "ario"--> doShift (myWorkspaces !! 4)
        , resource =? "ncmpcpp"--> doShift (myWorkspaces !! 4)
        , resource =? "alsamixer"--> doShift (myWorkspaces !! 4)
        , resource =? "mutt"--> doShift (myWorkspaces !! 5)
        , resource =? "irssi"--> doShift (myWorkspaces !! 5)
        , resource =? "centerim"--> doShift (myWorkspaces !! 5)
        , manageDocks]
newManageHook = myManageHook <+> manageHook defaultConfig 

--------------------------------------------------------------------------------------------------------------------
-- DZEN LOG RULES for workspace names, layout image, current program title
--------------------------------------------------------------------------------------------------------------------
myLogHook h = dynamicLogWithPP ( defaultPP
  {
      ppCurrent   = dzenColor green0 background . pad
    , ppVisible   = dzenColor red0 background .   pad
    , ppHidden    = dzenColor red0 background .   pad
    , ppHiddenNoWindows = dzenColor yellow0 background. pad
    , ppWsSep   = ""
    , ppSep     = "    "
    , ppLayout    = wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor white1 background .
        (\x -> case x of
          "Full"        ->  "^i(~/.xmonad/dzen2/layout_full.xbm)"
          "Spacing 5 ResizableTall" ->  "^i(~/.xmonad/dzen2/layout_tall.xbm)"
          "ResizableTall"     ->  "^i(~/.xmonad/dzen2/layout_tall.xbm)"
          "SimplestFloat"     ->  "^i(~/.xmonad/dzen2/mouse_01.xbm)"
          "Circle"      ->  "^i(~/.xmonad/dzen2/full.xbm)"
          _       ->  "^i(~/.xmonad/dzen2/grid.xbm)"
        ) 
--    , ppTitle =   wrap "^ca(1,xdotool key alt+shift+x)^fg(#222222)^i(~/.xmonad/dzen2/corner_left.xbm)^bg(#222222)^fg(#AADB0F)^fn(fkp)x^fn()" "^fg(#222222)^i(~/.xmonad/dzen2/corner_right.xbm)^ca()" .  dzenColor white0 "#222222" . shorten 40 . pad   
    , ppOrder =  \(ws:l:t:_) -> [ws,l]
    , ppOutput  =   hPutStrLn h
  } )


--------------------------------------------------------------------------------------------------------------------
-- Spawn pipes and menus on boot, set default settings
--------------------------------------------------------------------------------------------------------------------
myXmonadBar = "dzen2 -x '0' -y '0' -h '14' -w '1500' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc ~/.xmonad/.conky_dzen | dzen2 -x '1200' -w '666' -h '14' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont
--myConky = "conky -c ~/conkyrc"
--myStartMenu = "~/.xmonad/start ~/.xmonad/start_apps"


main = do
  dzenLeftBar   <- spawnPipe myXmonadBar
  dzenRightBar  <- spawnPipe myStatusBar
  xmproc    <- spawnPipe "GTK2_RC_FILES=~/.gtkdocky /usr/bin/docky"
  xmproc    <- spawnPipe "tint2 -c ~/.config/tint2/xmonad.tint2rc"
--  conky     <- spawn myConky
--  dzenStartMenu <- spawnPipe myStartMenu
  xmonad $ ewmh defaultConfig
    { terminal    = myTerminal
    , borderWidth   = 1
    , normalBorderColor   = yellow0
    , focusedBorderColor    = green0
    , modMask     = mod1Mask
    , layoutHook    = myLayout
    , workspaces    = myWorkspaces
    , manageHook    = newManageHook
    , handleEventHook   = fullscreenEventHook <+> docksEventHook
    , startupHook   = setWMName "LG3D"
    , logHook   = myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd
    }
--------------------------------------------------------------------------------------------------------------------
-- Keyboard options
--------------------------------------------------------------------------------------------------------------------
    `additionalKeys`
    [((mod1Mask .|. shiftMask , xK_b), spawn "chromium")
    ,((mod1Mask       , xK_b), spawn "dwb")
    ,((mod1Mask .|. shiftMask , xK_n), spawn "xterm -fn '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*' -fb '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*' -fi '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*'")
    ,((mod1Mask .|. shiftMask   , xK_t), spawn "xterm -e tmux")
    ,((mod4Mask       , xK_z), spawn "zathura")
    ,((mod4Mask       , xK_w), spawn "lowriter")
    ,((mod4Mask       , xK_c), spawn "localc")
    ,((mod4Mask       , xK_m), spawn "xterm -title mutt -name mutt -e muttb")
    ,((mod4Mask       , xK_i), spawn "xterm -title irssi -name irssi -e irssi")
    ,((mod4Mask       , xK_n), spawn "xterm -title ncmpcpp -name ncmpcpp -e ncmpcpp")
    ,((mod4Mask       , xK_a), spawn "xterm -title alsamixer -name alsamixer -e alsamixer")
    ,((mod4Mask       , xK_M), spawn "xterm -title centerim -name centerim -e centerim")
    ,((mod1Mask       , xK_r), spawn "~/scripts/lens")
    ,((mod1Mask .|. shiftMask , xK_r), spawn "~/scripts/dmenu/spotlight")
    ,((mod1Mask     , xK_q), spawn "killall dzen2; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
    ,((mod1Mask .|. shiftMask , xK_i), spawn "xcalib -invert -alter")
    ,((mod1Mask .|. shiftMask , xK_x), kill)
    ,((mod1Mask .|. shiftMask , xK_c), return())
    ,((mod1Mask       , xK_p), moveTo Prev NonEmptyWS)
    ,((mod1Mask       , xK_n), moveTo Next NonEmptyWS)
    ,((mod1Mask       , xK_c), moveTo Next EmptyWS)
    ,((mod1Mask .|. shiftMask , xK_l), sendMessage MirrorShrink)
    ,((mod1Mask .|. shiftMask , xK_h), sendMessage MirrorExpand)
    ,((mod1Mask       , xK_a), withFocused (keysMoveWindow (-20,0)))
    ,((mod1Mask       , xK_d), withFocused (keysMoveWindow (0,-20)))
    ,((mod1Mask       , xK_s), withFocused (keysMoveWindow (0,20)))
    ,((mod1Mask       , xK_f), withFocused (keysMoveWindow (20,0)))
    ,((mod1Mask .|. shiftMask   , xK_a), withFocused (keysResizeWindow (-20,0) (0,0)))
    ,((mod1Mask .|. shiftMask   , xK_d), withFocused (keysResizeWindow (0,-20) (0,0)))
    ,((mod1Mask .|. shiftMask   , xK_s), withFocused (keysResizeWindow (0,20) (0,0)))
    ,((mod1Mask .|. shiftMask   , xK_f), withFocused (keysResizeWindow (20,0) (0,0)))
    ,((0        , xK_Super_L), spawn "menu ~/.xmonad/apps")
    ,((mod1Mask     , xK_Super_L), spawn "menu ~/.xmonad/configs")
    ,((mod1Mask       , xK_F1), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_music.sh")
    ,((mod1Mask       , xK_F2), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_vol.sh")
    ,((mod1Mask       , xK_F3), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_network.sh")
    ,((mod1Mask       , xK_F4), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_battery.sh")
    ,((mod1Mask       , xK_F5), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_hardware.sh")
    ,((mod1Mask       , xK_F6), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_pacman.sh")
    ,((mod1Mask       , xK_F7), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_date.sh")
    ,((mod1Mask       , xK_F8), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_log.sh")
    ,((0        , xK_Print), spawn "scrot & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
    ,((mod1Mask         , xK_Print), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
    ,((0                      , xF86XK_AudioLowerVolume), spawn "amixer set Master 2- & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    ,((0                      , xF86XK_AudioRaiseVolume), spawn "amixer set Master 2+ & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    ,((0                      , xF86XK_AudioMute), spawn "amixer set Master toggle")
--    ,((0                      , xF86XK_Display), spawn "xrandr --newmode `cvt 1366 768 | tail -n1 | cut' ' -f2`; xrandr --addmode VGA1 1368x768_60.00; xrandr --output VGA1 --mode 1368x768_60.00")
    ,((0                      , xF86XK_Sleep), spawn "pm-suspend")
    ,((0                      , xF86XK_AudioPlay), spawn "ncmpcpp toggle")
    ,((0                      , xF86XK_AudioNext), spawn "ncmpcpp next")
    ,((0                      , xF86XK_AudioPrev), spawn "ncmpcpp prev")
    ,((0, 0x1008FF05), spawn "asus-kbd-backlight up"           ) -- XF86XK_MonBrightnessUp
    ,((0, 0x1008FF06), spawn "asus-kbd-backlight down"           ) -- XF86XK_MonBrightnessDown

    , ((0, 0x1008FF12), spawn "~/common/bin/pa-vol.sh mute"          ) -- XF86XK_AudioMute
    , ((0, 0x1008FF11), spawn "~/common/bin/pa-vol.sh minus"         ) -- XF86XK_AudioLowerVolume
    , ((0, 0x1008FF13), spawn "~/common/bin/pa-vol.sh plus"          ) -- XF86XK_AudioRaiseVolume

    , ((0, 0x1008FF2C), spawn "eject"                                ) -- XF86XK_Eject
    , ((0, 0x1008ff2a), spawn "sudo pm-suspend"                      ) -- XF86XK_PowerOff
    ]
    `additionalMouseBindings`
    [((mod1Mask     , 6), (\_ -> moveTo Next NonEmptyWS))
    ,((mod1Mask     , 7), (\_ -> moveTo Prev NonEmptyWS))
    ,((mod1Mask     , 5), (\_ -> moveTo Prev NonEmptyWS))
    ,((mod1Mask     , 4), (\_ -> moveTo Next NonEmptyWS))
    ]



-- Define constants

myTerminal  = "xterm"
myBitmapsDir  = "~/.xmonad/dzen2/"
--myFont    = "-*-tamsyn-medium-*-normal-*-10-*-*-*-*-*-*-*"
--myFont    = "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
--myFont    = "-*-lime-*-*-*-*-*-*-*-*-*-*-*-*"
myFont    = "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont    = "'sans:italic:bold:underline'"
--myFont    = "xft:droid sans mono:size=9"
--myFont    = "xft:Droid Sans:size=12"
--myFont    = "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"

foreground= "#D3D3D3"
background= "#111111"

black0= "#181818"
black1= "#181818"
red0= "#D7D7D7"
red1= "#D7D7D7"
green0= "#AADB0F"
green1= "#AADB0F"
--green0= "#A80036"
--green1= "#A80036"
--green0= "#E2791B"
--green1= "#E2791B"
yellow0= "#666666"
yellow1= "#666666"
blue0= "#FFFFFF"
blue1= "#FFFFFF"
--magenta0= "#91BA0D"
--magenta1= "#91BA0D"
--magenta0= "#740629"
--magenta1= "#740629"
magenta0= "#BF3C0A"
magenta1= "#BF3C0A"
cyan0= "#D4D4D4"
cyan1= "#D4D4D4"
white0= "#D3D3D3"
white1= "#D3D3D3"
