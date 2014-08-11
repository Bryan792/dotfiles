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
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.FlexibleResize as FlexibleResize
import XMonad.Util.Run (spawnPipe)
import XMonad.Actions.CycleWS     -- nextWS, prevWS
import Data.List      -- clickable workspaces
import Data.Ratio ((%))   -- clickable workspaces

import XMonad.Layout.Reflect
--------------------------------------------------------------------------------------------------------------------
-- DECLARE WORKSPACES RULES
--------------------------------------------------------------------------------------------------------------------
code  = (myWorkspaces !! 0)
web   = (myWorkspaces !! 1)
chat  = (myWorkspaces !! 2)
floats= (myWorkspaces !! 3)
game  = (myWorkspaces !! 4)
docs  = (myWorkspaces !! 5)
tunes = (myWorkspaces !! 6)
mail  = (myWorkspaces !! 7)

myLayout = onWorkspace code (avoidStruts (tiledSpace ||| tiled) ||| fullTile)
    $ onWorkspace web (avoidStruts (tiledSpace ||| fullTile) ||| fullScreen)
    $ onWorkspace floats (avoidStruts simplestFloat)
    $ onWorkspace chat (avoidStruts pidginLayout)
    $ onWorkspace game (avoidStruts fullScreen)
    $ avoidStruts ( tiledSpace  ||| tiled ||| fullTile ) 
  where
    tiled     = spacing 5 $ ResizableTall nmaster delta ratio [] 
    tiledSpace    = spacing 60 $ ResizableTall nmaster delta ratio [] 
    fullScreen  = noBorders(fullscreenFull Full)
    fullTile  = ResizableTall nmaster delta ratio [] 
    borderlessTile  = noBorders(fullTile)
    -- Default number of windows in master pane
    nmaster = 1
    -- Percent of the screen to increment when resizing
    delta   = 5/100
    -- Default proportion of the screen taken up by main pane
    ratio   = toRational (2/(1 + sqrt 5 :: Double)) 
    gridLayout = spacing 8 $ Grid
    pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
    irssi = (Resource "irssi")
    steamRoster = And (ClassName "Steam") (Title "Friends")
    isSkype = (Title "bryanchaching - Skype™")
    pidginLayout = withIM (1%9) pidginRoster $ withIM (1%8) isSkype $ withIM (1%7) steamRoster $ reflectHoriz $ withIM (0.7) irssi gridLayout 


--------------------------------------------------------------------------------------------------------------------
-- WORKSPACE DEFINITIONS
--------------------------------------------------------------------------------------------------------------------
myWorkspaces = clickable $ ["code"
    ,"web"  
    ,"chat"
    ,"float"
    ,"game"
    ,"docs" 
    ,"tunes"
    ,"mail"]
 	where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
				(i,ws) <- zip [1..] l,
				let n = i ]

--------------------------------------------------------------------------------------------------------------------
-- APPLICATION SPECIFIC RULES
--------------------------------------------------------------------------------------------------------------------
myManageHook = composeAll   [ resource =? "dmenu" --> doFloat
        , isFullscreen --> doFullFloat
--        , className =? "emulator64-x86" --> doFloat
        , resource =? "skype"   --> doFloat
        , resource =? "skype"   --> doShift chat
        , resource =? "mplayer" --> doFloat
        , resource =? "feh" --> doFloat
        , resource =? "chromium"--> doShift web
        , resource =? "google-chrome"--> doShift web 
        , className =? "Pidgin"--> doShift chat 
        , className =? "Steam"--> doShift chat 
        , className =? "dota_linux"--> doShift game 
        , className =? "Eclipse"--> doShift code 
        , resource =? "lowriter"--> doShift floats
        , resource =? "localc"--> doShift floats
        , resource =? "loimpress"--> doShift floats
        , resource =? "zathura"--> doShift floats
        , resource =? "ario"--> doShift floats 
        , resource =? "ncmpcpp"--> doShift floats
        , resource =? "alsamixer"--> doShift floats 
        , resource =? "mutt"--> doShift (myWorkspaces !! 5)
        , resource =? "irssi"--> doShift chat 
        , manageDocks]
newManageHook = myManageHook <+> manageHook defaultConfig 

--------------------------------------------------------------------------------------------------------------------
-- DZEN LOG RULES for workspace names, layout image, current program title
--------------------------------------------------------------------------------------------------------------------
myLogHook h = dynamicLogWithPP ( defaultPP
	{
		  ppCurrent		= dzenColor color9 background .	pad
		, ppVisible		= dzenColor color6 background . 	pad
	  , ppUrgent = dzenColor "#FF0000" "" . wrap "*" "*"
		, ppHidden		= dzenColor color6 background . 	pad
		, ppHiddenNoWindows	= dzenColor color0 background .	pad
		, ppWsSep		= ""
		, ppSep			= "    "
		, ppLayout		= wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor foreground background .
				(\x -> case x of
					"Full"				->	"^i(/home/bryan/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 5 ResizableTall"	->	"^i(/home/bryan/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/bryan/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/bryan/.xmonad/dzen2/mouse_01.xbm)"
					"Circle"			->	"^i(/home/bryan/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/bryan/.xmonad/dzen2/grid.xbm)"
				) 
--		, ppTitle	=   wrap "^ca(1,xdotool key alt+shift+x)^fg(#7e7175)^i(/home/bryan/.xmonad/dzen2/corner_left.xbm)^bg(#7e7175)^fg(#f92671)^fn(fkp)x^fn()" "^fg(#7e7175)^i(/home/bryan/.xmonad/dzen2/corner_right.xbm)^ca()" .  dzenColor foreground "#7e7175" . shorten 40 . pad		
		, ppTitle	=  wrap "^ca(1,xdotool key alt+shift+x)^fg(#D23D3D)^fn(fkp)x ^fn()" "^ca()" . dzenColor foreground background . shorten 40 . pad
		, ppOrder	=  \(ws:l:t:_) -> [ws,l, t]
		, ppOutput	=   hPutStrLn h
	} )


--------------------------------------------------------------------------------------------------------------------
-- Spawn pipes and menus on boot, set default settings
--------------------------------------------------------------------------------------------------------------------
myXmonadBar = "dzen2 -x '0' -y '0' -h '14' -w '1100' -ta 'l' -fg '"++foreground++"' -bg '"++background++"' -fn "++myFont
myStatusBar = "conky -qc /home/bryan/.xmonad/.conky_dzen | dzen2 -x '1100' -w '700' -h '14' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont
--myStatusBar = "conky -qc /home/bryan/.xmonad/.conky_dzen | dzen2 -x '600' -w '766' -h '14' -ta 'r' -bg '"++background++"' -fg '"++foreground++"' -y '0' -fn "++myFont
--myConky = "conky -c /home/bryan/conkyrc"
--myStartMenu = "/home/bryan/.xmonad/start /home/bryan/.xmonad/start_apps"


main = do
  dzenLeftBar   <- spawnPipe myXmonadBar
  dzenRightBar  <- spawnPipe myStatusBar
--  xmproc    <- spawnPipe "/usr/bin/docky"
--  xmproc    <- spawnPipe "GTK2_RC_FILES=/home/bryan/.gtkrc-2.0 /usr/bin/docky"
  xmproc    <- spawnPipe "tint2 -c /home/bryan/.config/tint2/xmonad.tint2rc"
--  conky     <- spawn myConky
--  dzenStartMenu <- spawnPipe myStartMenu
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh defaultConfig
    { terminal    = myTerminal
    , borderWidth   = 2
    , normalBorderColor   = background
    , focusedBorderColor    = color9
    , modMask     = mod1Mask
    , layoutHook    = smartBorders $ myLayout
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
    [((mod1Mask .|. shiftMask , xK_b), spawn "google-chrome")
    ,((mod1Mask       , xK_b), spawn "dwb")
--    ,((mod1Mask .|. shiftMask , xK_n), spawn "urxvtc -fn '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*' -fb '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*' -fi '-*-gohufont-medium-r-normal-*-12-*-*-*-*-*-*-*'")
    ,((mod1Mask .|. shiftMask , xK_n), spawn "urxvtc")
    ,((mod1Mask .|. shiftMask   , xK_t), spawn "urxvtc -e tmux")
    ,((mod4Mask       , xK_z), spawn "zathura")
    ,((mod4Mask       , xK_w), spawn "lowriter")
    ,((mod4Mask       , xK_c), spawn "localc")
    ,((mod4Mask       , xK_m), spawn "urxvtc -title mutt -name mutt -e muttb")
    ,((mod4Mask       , xK_i), spawn "urxvtc -title irssi -name irssi -e ~/.irssi/tmux_irssi.sh")
    ,((mod4Mask       , xK_n), spawn "urxvtc -title ncmpcpp -name ncmpcpp -e ncmpcpp")
    ,((mod4Mask       , xK_a), spawn "urxvtc -title alsamixer -name alsamixer -e alsamixer")
    ,((mod4Mask       , xK_M), spawn "urxvtc -title centerim -name centerim -e centerim")
    ,((mod1Mask       , xK_r), spawn "/home/bryan/scripts/lens")
    ,((mod1Mask .|. shiftMask , xK_r), spawn "/home/bryan/scripts/dmenu/spotlight")
    ,((mod1Mask     , xK_q), spawn "killall dzen2; killall conky; killall tint2; killall docky; cd /home/bryan/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
    ,((mod4Mask     , xK_q), spawn "killall dzen2; killall conky; killall tint2; killall docky" )
    ,((mod1Mask .|. shiftMask , xK_i), spawn "xcalib -invert -alter")
    ,((mod1Mask .|. shiftMask , xK_x), kill)
    ,((mod1Mask .|. shiftMask , xK_c), return())
    ,((mod1Mask       , xK_p), moveTo Prev NonEmptyWS)
    ,((mod1Mask       , xK_n), moveTo Next NonEmptyWS)
    ,((mod1Mask       , xK_c), moveTo Next EmptyWS)
    ,((mod1Mask .|. controlMask, xK_Left  ), prevWS)
    ,((mod1Mask .|. controlMask, xK_Right ), nextWS)
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
    ,((0        , xK_Super_L), spawn "menu /home/bryan/.xmonad/apps")
    ,((mod1Mask     , xK_Super_L), spawn "menu /home/bryan/.xmonad/configs")
    ,((mod1Mask       , xK_F1), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_music.sh")
    ,((mod1Mask       , xK_F2), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_date.sh")
    ,((mod1Mask       , xK_F3), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_network.sh")
    ,((mod1Mask       , xK_F4), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_vol.sh")
    ,((mod1Mask       , xK_F5), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_battery.sh")
    ,((mod1Mask       , xK_F6), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_hardware.sh")
    ,((mod1Mask       , xK_F7), spawn "~/.xmonad/sc ~/.xmonad/scripts/dzen_log.sh")
    ,((0        , xK_Print), spawn "scrot & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
    ,((mod1Mask       , xK_Print), spawn "scrot -s & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
    ,((0                      , 0x1008FF11), spawn "/home/bryan/.xmonad/dvol.sh -d 1 & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    ,((0                      , 0x1008FF13), spawn "/home/bryan/.xmonad/dvol.sh -i 1 & mplayer /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    ,((0                      , 0x1008FF12), spawn "/home/bryan/.xmonad/dvol.sh -t")
    ,((0                      , xF86XK_Display), spawn "/home/bryan/scripts/project")
    ,((0                      , xF86XK_Sleep), spawn "pm-suspend")
    ,((0                      , xF86XK_AudioPlay), spawn "ncmpcpp toggle")
    ,((0                      , xF86XK_AudioNext), spawn "ncmpcpp next")
    ,((0                      , xF86XK_AudioPrev), spawn "ncmpcpp prev") 
    ,((0                      , 0x1008ffa9), spawn "/usr/bin/synclient TouchpadOff=`expr $(synclient -l | grep TouchpadOff | grep -o .$) = 0`")
    ,((0, 0x1008FF05), spawn "asus-kbd-backlight up"           ) -- XF86XK_MonBrightnessUp
    ,((0, 0x1008FF06), spawn "asus-kbd-backlight down"           ) -- XF86XK_MonBrightnessDown

    , ((0, 0x1008FF2C), spawn "eject"                                ) -- XF86XK_Eject
    , ((0, 0x1008ff2a), spawn "sudo pm-suspend"                      ) -- XF86XK_PowerOff
    ]
    `additionalMouseBindings`
    [((mod1Mask     , 6), (\_ -> moveTo Next NonEmptyWS))
    ,((mod1Mask     , 7), (\_ -> moveTo Prev NonEmptyWS))
    ,((mod1Mask     , 5), (\_ -> moveTo Prev NonEmptyWS))
    ,((mod1Mask     , 4), (\_ -> moveTo Next NonEmptyWS))
--    ,((0        , 2), (\w -> focus w >> windows W.swapMaster))
--    ,((0        , 3), (\w -> focus w >> FlexibleResize.mouseResizeWindow w))
    ]



-- Define constants

myTerminal  = "urxvtc"
myBitmapsDir  = "~/.xmonad/dzen2/"
--myFont    = "-*-tamsyn-medium-*-normal-*-10-*-*-*-*-*-*-*"
--myFont    = "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
--myFont    = "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont      = "-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont      = "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont    = "'sans:italic:bold:underline'"
--myFont    = "xft:droid sans mono:size=9"
--myFont    = "xft:Droxd Sans:size=12"
--myFont    = "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"

--myFont    = "-*-tamsyn-medium-r-normal-*-12-87-*-*-*-*-*-*"
--myFont    = "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
myFont    = "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*"
--myFont      = "-artwiz-lime-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont      = "-artwiz-limey-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont    = "-benis-lemon-medium-r-normal-*-10-110-75-75-m-50-iso8859-*"
--myFont    = "'sans:italic:bold:underline'"
--myFont    = "xft:droid sans mono:size=9"
--myFont    = "xft:Droxd Sans:size=12"
--myFont    = "-*-cure-*-*-*-*-*-*-*-*-*-*-*-*"



background= "#181512"
foreground= "#bea492"

color0= "#332d29"
color8= "#817267"

color1= "#8c644c"
color9= "#9f7155"

color2= "#c4be90"
color10= "#bec17e"

color3= "#bfba92"
color11= "#fafac0"

color4= "#646a6d"
color12= "#626e74"

color5= "#6d6871"
color13= "#756f7b"

color6= "#3b484a"
color14= "#444d4e"

color7= "#504339"
color15= "#9a875f"



--background=            "#262729"
--foreground=            "#f8f8f2"
--color0=                "#626262"
--color8=                "#626262"
--color1=                "#f92671"
--color9=                "#ff669d"
--color2=                "#a6e22e"
--color10=               "#beed5f"
--color3=                "#fd971f"
--color11=               "#e6db74"
--color4=                "#1692d0"
--color12=               "#66d9ef"
--color5=                "#9e6ffe"
--color13=               "#df92f6"
--color6=                "#5e7175"
--color14=               "#a3babf"
--color7=                "#ffffff"
--color15=               "#ffffff"
--cursorColor=           "#b5d2dd"
--
--color4 = "#6fa5df"
--color12 = "#a8c9eb"
--
--color5 = "#846fdf"
--color13 = "#b5a8eb"
--
--color6 = "#6fcadf"
--color14 = "#a6e6eb"
--
--color7 = "#aaaaaa"
--color15 = "#c3c3c3"

