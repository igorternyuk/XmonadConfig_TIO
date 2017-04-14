import XMonad   hiding ( (|||) ) 

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.SetWMName
import XMonad.Hooks.XPropManage
import XMonad.Config.Xfce
import XMonad.Actions.MouseGestures
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Search
import XMonad.Actions.WindowGo
import XMonad.Actions.Promote
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.FlexibleResize as FlexR
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SinkAll
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.RotSlaves
import XMonad.Actions.FloatKeys
import XMonad.Actions.WindowBringer
import XMonad.Actions.TagWindows

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.Font
import XMonad.Util.XSelection
import XMonad.Util.WorkspaceCompare
import XMonad.Util.WindowProperties
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Themes

import XMonad.ManageHook

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xlib.Extras
import Foreign.C.Types (CLong)

import XMonad.Layout.Named
import XMonad.Layout.LayoutCombinators 
import XMonad.Layout.WindowNavigation 
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Maximize  
import XMonad.Layout.StackTile  
import XMonad.Layout.LayoutHints  
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders 
import XMonad.Layout.Gaps 
import XMonad.Layout.Circle 
import XMonad.Layout.Cross 
import XMonad.Layout.Spacing 
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane 
import XMonad.Layout.Tabbed 
import XMonad.Layout.Accordion 
import XMonad.Layout.Grid 
import XMonad.Layout.NoBorders 
import XMonad.Layout.PerWorkspace 
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat 
import XMonad.Layout.ResizableTile 
import XMonad.Layout.IM 
import XMonad.Layout.Reflect 
import XMonad.Layout.SimpleDecoration 
import XMonad.Layout.DwmStyle 
import XMonad.Layout.WindowArranger 
import XMonad.Layout.Mosaic
import XMonad.Layout.Combo
import XMonad.Layout.Roledex 
import XMonad.Layout.Dishes
import XMonad.Layout.Spiral

import XMonad.Prompt 
import XMonad.Prompt.Man
import XMonad.Prompt.Ssh
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.AppLauncher as AL
import XMonad.Prompt.Layout
import XMonad.Util.Run

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.ConstrainedResize as SQR
import qualified XMonad.Actions.FlexibleResize as FlexR

import Data.Monoid
import Data.Ratio ((%))
import Data.List 

import Control.Monad (liftM2)
import XMonad.Operations

import System.Exit
import System.IO

--import qualified Control.Browser.chromium-browser as CF
--import Network
import qualified Data.ByteString.Char8 as B

-- Named workspaces

myWorkspaces = ["\xf115", "1:\xf17a","2:\xf0ac","3:\xf02d","4:\xf044","5:prg","6:\xf0e3","7:\xf085","8:\xf0e0","esc:\xf120", "F1:\xf03e","F2:\xf008","F3:\xf025","F4:\xf17e"]
myModMask = mod4Mask
myTerminal = "xterm"
myFont = "-xos4-terminus-medium-r-normal--12-120-72-72-c-60-*-*"
myBorderWidth = 2 
--myNormalBorderColor  = "snow4" #85a969
myNormalBorderColor  = "#85a969"
myFocusedBorderColor = "#4c90b1" 
myEventHook = floatClickFocusHandler

-- bring clicked floating window to the front
floatClickFocusHandler :: Event -> X All
floatClickFocusHandler ButtonEvent { ev_window = w } = do
  withWindowSet $ \s -> do
    if isFloat w s
       then (focus w >> promote)
       else return ()
    return (All True)
    where isFloat w ss = M.member w $ W.floating ss
floatClickFocusHandler _ = return (All True)

myDeco = defaultTheme    { 
                             activeColor         = "darkgreen"
                           , inactiveColor       = "gray"
                           , urgentColor         = "darkred"
                           , activeBorderColor   = "black"
                           , inactiveBorderColor = "black"
                           , urgentBorderColor   = "yellow"
                           , activeTextColor     = "black"
                           , inactiveTextColor   = "black"
                           , urgentTextColor     = "yellow"
                           , decoHeight          = 10 }

tabConfig = defaultTheme {
      fontName = "xft:PragmataPro:pixelsize=14"
    , activeBorderColor = "#fd0019"
    , activeTextColor   = "#3af75b"
    , activeColor       = "#365434"
    , inactiveBorderColor = "#7C7C7C"
    , inactiveTextColor   = "#4c90b1"
    , inactiveColor       = "#000000"
}

-- decoration for launcher

myXPConfig = defaultXPConfig {
             position    = Top
           , font = "xft:Droid Sans Mono:size=16"
           , height      = 24
           , bgColor     = "#3F3F3F"
           , fgColor     = "#EFEFEF"
           , fgHLight    = "#000D18"
           , bgHLight    = "#8FAF9F"
           , borderColor = "#719E7F"
           , historySize = 512
           , showCompletionOnTab = True
           , historyFilter = deleteConsecutive
           }

myDWConfig = defaultTheme {

      fontName = "xft:Droid Sans Mono:size=16"
    , activeBorderColor = "#fd0019"
    , activeTextColor   = "#CEFFAC"
    , activeColor       = "#000000"
    , inactiveBorderColor = "#7C7C7C"
    , inactiveTextColor   = "#EEEEEE"
    , inactiveColor       = "#000000"
}

gsconfig1 = defaultGSConfig { 
    gs_cellheight  = 50
  , gs_cellwidth   = 200
  , gs_cellpadding = 10
  , gs_font        = "xft:Droid Sans:size=14"
}
gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { 
    gs_cellheight  = 50
  , gs_cellwidth   = 200
  , gs_cellpadding = 10
  , gs_font        = "xft:Droid Sans:size=14"
}

scratchpads = [

  --NS "terminal1" "xterm -title terminal1"                                       (title =? "terminal1") middleRect,
  NS "terminal1" "xfce4-terminal --role scratchpad1"                            (role =? "scratchpad1") middleRect,
  NS "terminal2" "xfce4-terminal --role scratchpad2"                            (role =? "scratchpad2") middleRect,
  NS "terminal3" "xfce4-terminal --role scratchpad3"                            (role =? "scratchpad3") middleRect,
  NS "ranger" "xterm -title ranger -e ranger"                                   (title =? "ranger") largeRect,
 -- NS "Vim"    "xterm -title Vim -e vim"                                         (title =? "Vim") largeRect,
  NS "vim" "xfce4-terminal --role vim -e vim"                                    (role =? "vim") largeRect,
  NS "python" "xterm -title python -e python"                                   (title =? "python") middleRect,
  NS "mocp" "xfce4-terminal --role mocp -e mocp"                                 (role =? "mocp") largeRect,
  --NS "moc" "xterm -title moc -e mocp"                                           (title =? "moc") largeRect,  
  NS "mc" "roxterm --role mc -e mc"                                             (role =? "mc") largeRect, 
  NS "htop" "xterm -title htop -e htop"                                         (title =? "htop") smallRect,
  NS "alsamixer" "xterm -title alsamixer -e alsamixer"                          (title =? "alsamixer") smallRect,
  NS "lynx" "xterm -title lynx -e lynx"                                         (title =? "lynx") largeRect,  
  --NS "alpine" "xterm -title alpine -e alpine"                                   (title =? "alpine") largeRect,

-- GUI apps
      
  NS "cherrytree" "cherrytree"                           (className =? "Cherrytree") largeRect,
  NS "dolphin" "dolphin"                                 (className =? "Dolphin") largeRect,
  NS "goldendict" "goldendict"                           (className =? "Goldendict") middleRect,
  NS "transmission-gtk" "transmission-gtk"               (className =? "Transmission-gtk")  middleRect, 
  NS "speedcrunch" "speedcrunch"                         (className =? "Speedcrunch")  middleRect, 
  NS "gnome-search-tool" "gnome-search-tool"             (className =? "gnome-search-tool") middleRect,
  NS "nitrogen" "nitrogen"                               (className =? "Nitrogen") smallRect,
  NS "qtcreator1" "/home/igor/Qt5.7.0/Tools/QtCreator/bin/qtcreator" (className =? "QtCreator") largeRect
  ]
  where role = stringProperty "WM_WINDOW_ROLE"
        largeRect = (customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10))
        middleRect = (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
        smallRect = (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
	
-- Dialog and menu windows

-- Взять значение свойства окна
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- Эта функция проверяет, выставлено ли свойство окна name в значение value

checkAtom name value = ask >>= \w -> liftX $ do
          a <- getAtom name
          val <- getAtom value
          mbr <- getProp a w
          case mbr of
            Just [r] -> return $ elem (fromIntegral r) [val]
            _ -> return False
            
-- Эта функция проверяет, является ли окно диалогом

checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

checkMenu = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"

-- Сделать меню плавающими
manageMenus = checkMenu --> doFloat
-- Сделать диалоги плавающими
manageDialogs = checkDialog --> doFloat

myKeys conf@(XConfig {XMonad.modMask = mod4Mask}) = M.fromList $
    [
-- gridselect
       -- Switching between running applications
         ((mod4Mask, xK_a), windowPromptGoto myXPConfig)
       --, ((mod4Mask, xK_g), goToSelected defaultGSConfig)
       --, ((mod4Mask, xK_g), goToSelected $ myGSConfig myColorizer)
       , ((mod1Mask, xK_Tab), spawn "xwinmosaic")
       ---------------------------------------------------
       ,((mod1Mask, xK_g), goToSelected  $ gsconfig2 defaultColorizer)
       --,((mod4Mask, xK_p), spawnSelected $ spawnSelected defaultColorizer)
       --launchers
    , ((mod4Mask .|. shiftMask, xK_p), spawn "dmenu_run -fn -*-terminus-*-r-*-*-20-*-*-*-*-*-*-* -nb bisque3 -nf darkgreen -sb blue -sf red")
    , ((mod1Mask, xK_F2), spawn "gmrun")
    --, ((mod4Mask .|. mod1Mask, xK_p), shellPrompt myXPConfig) 
    , ((mod4Mask, xK_r), spawn "xwinmosaic_run")
-- Запуск наиболее часто используемых програм

     , ((mod4Mask, xK_w), spawnSelected gsconfig1 [
          "spacefm","cherrytree","chromium-browser","opera", "firefox", "leafpad","xfce4-terminal",
          "skype", "deadbeaf", "vlc","cheese", "gwenview", "libreoffice","wps", "gnumeric",
          "djview4", "dia", "pinta", "gimp","lookit", "virtualbox","lxappearance","baobab"])
-- terminals

    , ((mod4Mask .|. shiftMask, xK_Return), spawn "xterm")
    , ((mod4Mask , xK_Return), spawn "terminator")
    --, ((mod4Mask , xK_Return), spawn "xterm") 
    --, ((mod1Mask .|. controlMask, xK_Return), spawn "xfce4-terminal")
    --Wallpapers
    --, ((mod4Mask, xK_s), spawn "feh --auto-zoom --full-screen --hide-pointer --randomize --slideshow-delay 5 --title \"feh() | %n\" /home/and1/wallpapers/*.jpg /home/and1/wallpapers/*.png")
    --, ((mod4Mask, xK_z), spawn "feh --auto-zoom --full-screen --hide-pointer --randomize --slideshow-delay 10 --title \"feh() | %n\" /home/igor/Pictures/Wallpapers/xmonad/*.jpg /home/igor/Pictures/Wallpapers/xmonad/*.png")
    --, ((mod4Mask, xK_s), spawn "xfeh --bg-fill /home/igor/Pictures/Xwallpapers/xmonad.png"
--windows

    , ((mod4Mask .|. shiftMask, xK_c), kill)
    , ((mod1Mask, xK_F4), kill)
    , ((mod4Mask, xK_space), sendMessage NextLayout)
    , ((mod4Mask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    , ((mod4Mask, xK_Tab), windows W.focusDown) 
    , ((mod4Mask .|. controlMask, xK_Tab), cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)
    , ((mod4Mask .|. shiftMask, xK_Tab), windows W.focusUp)
    , ((mod4Mask, xK_k), windows W.focusUp)
    , ((mod4Mask, xK_j), windows W.focusDown)
    , ((mod4Mask, xK_m), windows W.focusMaster)
    , ((mod4Mask, xK_Return), windows W.swapMaster)
    , ((mod4Mask .|. shiftMask, xK_j), windows W.swapDown)
    , ((mod4Mask .|. shiftMask, xK_k), windows W.swapUp)
    , ((mod4Mask .|. controlMask, xK_Right), nextWS)
    , ((mod4Mask .|. shiftMask, xK_Right), shiftToNext)
    , ((mod4Mask .|. controlMask, xK_Left), prevWS)
    , ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev)

--XMonad.Layout.WindowNavigation   
 
    , ((mod1Mask .|. controlMask, xK_Right), sendMessage $ Move R)
    , ((mod1Mask .|. controlMask, xK_Left), sendMessage $ Move L)
    , ((mod1Mask .|. controlMask, xK_Up), sendMessage $ Move U)
    , ((mod1Mask .|. controlMask, xK_Down), sendMessage $ Move D)

-- Moving the floated windows

    , ((mod4Mask .|. mod1Mask, xK_Left), withFocused (keysMoveWindow (-30,0))) -- move floated window 10 pixels left
    , ((mod4Mask .|. mod1Mask, xK_Right), withFocused (keysMoveWindow (30,0))) -- move floated window 10 pixels right
    , ((mod4Mask .|. mod1Mask, xK_Up), withFocused (keysMoveWindow (0,-30)))   -- move floated window 10 pixels up
    , ((mod4Mask .|. mod1Mask, xK_Down), withFocused (keysMoveWindow (0,30)))  -- move floated window 10 pixels down
    
 --JumpToLayout
 
    , ((mod1Mask .|. controlMask, xK_f), sendMessage $ JumpToLayout "myFull")
    , ((mod1Mask .|. controlMask, xK_z), sendMessage $ JumpToLayout "myTabs2")
    , ((mod1Mask .|. controlMask, xK_c), sendMessage $ JumpToLayout "myCross")
    , ((mod1Mask .|. controlMask, xK_o), sendMessage $ JumpToLayout "myCircle")
    , ((mod1Mask .|. controlMask, xK_r), sendMessage $ JumpToLayout "myGoldRatioTall")
    , ((mod1Mask .|. controlMask, xK_1), sendMessage $ JumpToLayout "myTabsAndFull")
    , ((mod1Mask .|. controlMask, xK_2), sendMessage $ JumpToLayout "myLayout3")
    , ((mod1Mask .|. controlMask, xK_3), sendMessage $ JumpToLayout "myThreeColMid")
    , ((mod1Mask .|. controlMask, xK_4), sendMessage $ JumpToLayout "myRoledex")
    , ((mod1Mask .|. controlMask, xK_d), sendMessage $ JumpToLayout "myDishes") 
    , ((mod1Mask .|. controlMask, xK_s), sendMessage $ JumpToLayout "mySpiral") 
    
-- Shrink/Expand the master area

    , ((mod4Mask, xK_h), sendMessage Shrink)
    , ((mod4Mask, xK_l), sendMessage Expand)
    , ((mod4Mask .|. shiftMask, xK_h), sendMessage MirrorShrink)
    , ((mod4Mask .|. shiftMask, xK_l), sendMessage MirrorExpand)

    -- toggle focused window fullscreen

    --, ((mod4Mask, xK_f), withFocused (sendMessage . maximizeRestore))
    , ((mod4Mask, xK_f), sendMessage $ Toggle FULL)

    -- toggle focused window to the tiling mode

    , ((mod4Mask, xK_t), withFocused $ windows . W.sink)

 -- Swap the focused window and the master window

    , ((mod4Mask, xK_semicolon), windows W.swapMaster)
    , ((mod4Mask, xK_comma), sendMessage (IncMasterN 1))
    , ((mod4Mask, xK_period), sendMessage (IncMasterN (-1)))
    --, ((mod4Mask, xK_r), refresh)

-- show/hide xmobar

    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_g), sendMessage $ ToggleGaps)  -- toggle all gaps

--Keybindings for mosaic layout 

    --, ((mod4Mask , xK_a), sendMessage Taller)
    , ((mod4Mask .|. controlMask , xK_z), sendMessage Taller)
    , ((mod4Mask , xK_z), sendMessage Wider)
   -- , ((mod4Mask .|. controlMask, xK_r), sendMessage Reset)

--reflect

    , ((mod4Mask, xK_x), sendMessage $ Toggle REFLECTX)
    , ((mod4Mask, xK_y), sendMessage $ Toggle REFLECTY)

--screensavers

    --, ((mod1Mask .|. controlMask, xK_l), spawn "gnome-screensaver-command --lock") 
    , ((mod4Mask, xK_Pause ), spawn "slock")

--screenshots
     --take a screenshot of entire display
    ,((0, xK_Print), spawn "gnome-screenshot")
     -- take screenshot wirh 3s delay
    ,((mod4Mask, xK_Print), spawn "scrot -q300 '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/screenshots/' && sleep 3 && notify-send \"ScreenShot Done \"")
     --take a screenshot with differen options
    ,((controlMask, xK_Print), spawn "gnome-screenshot -i")
     --take a screenshot of the active window 
    ,((mod1Mask, xK_Print), spawn "scrot -q300 '%Y-%m-%d-%H%M_$wx$h.png' --focused -e 'mv $f ~/screenshots/'")
    --take a screenshot of selected area  
    ,((shiftMask, xK_Print), spawn "xfce4-screenshooter -r -s ~/screenshots")

-- layouts

    ,((mod4Mask .|. mod1Mask, xK_1), spawn "setxkbmap -layout 'us,ru' -option grp:alt_shift_toggle,grp_led:scroll") 
    ,((mod4Mask .|. mod1Mask, xK_2), spawn "setxkbmap -layout 'us,ua' -option grp:alt_shift_toggle,grp_led:scroll")
    ,((mod4Mask .|. mod1Mask, xK_3), spawn "setxkbmap -layout 'de,ru' -option grp:alt_shift_toggle,grp_led:scroll")
    ,((mod4Mask .|. mod1Mask, xK_4), spawn "setxkbmap -layout 'es,ru' -option grp:alt_shift_toggle,grp_led:scroll")
    ,((mod4Mask .|. mod1Mask, xK_5), spawn "setxkbmap -layout 'pt,ru' -option grp:alt_shift_toggle,grp_led:scroll")
    ,((mod4Mask .|. mod1Mask, xK_6), spawn "setxkbmap -layout 'it,ru' -option grp:alt_shift_toggle,grp_led:scroll")

--soft
     -- web-browsers
     , ((mod4Mask .|. shiftMask, xK_b), spawn "opera")
     , ((mod4Mask .|. shiftMask, xK_f), spawn "firefox")                
     -- file managers
     , ((mod4Mask .|. shiftMask, xK_t), spawn "/opt/sublime_text/sublime_text")
     , ((mod4Mask .|. mod1Mask, xK_f), spawn "spacefm")
     , ((mod4Mask , xK_e), spawn "thunar /home/igor")
     , ((mod1Mask , xK_k), spawn "krusader")
      ,((mod4Mask .|. shiftMask .|. controlMask, xK_j), setWMName "LG3D") -- @@ Java hack
     -- audio players
     , ((mod4Mask .|. shiftMask, xK_d), spawn "deadbeef")
     , ((mod4Mask .|. shiftMask, xK_a), spawn "audacious")
     -- video players
     , ((mod4Mask .|. mod1Mask, xK_v), spawn "vlc")
     , ((mod4Mask .|. mod1Mask, xK_y), spawn "smtube")
     , ((mod4Mask .|. mod1Mask, xK_s), spawn "smplayer")
     -- text editors
     -- ofice
     , ((mod4Mask .|. shiftMask, xK_g), spawn "gimp")
     , ((mod4Mask .|. shiftMask, xK_o), spawn "libreoffice")
     -- document readers
     , ((mod4Mask .|. mod1Mask, xK_k), spawn "djview4")
     -- grafical applications
     , ((mod4Mask .|. mod1Mask, xK_p), spawn "pinta")
     , ((mod4Mask .|. controlMask, xK_g), spawn "gnumeric")
     , ((mod4Mask .|. mod1Mask, xK_i), spawn "inkscape")
     --chat
     , ((mod4Mask .|. shiftMask, xK_s), spawn "bash -c 'LD_PRELOAD=/usr/lib/i386-linux-gnu/libv4l/v4l1compat.so skype'")
     , ((mod4Mask, xK_v), spawn "/opt/viber/Viber")
      --web-camera
     , ((mod4Mask, xK_c), spawn "cheese")
     -- settings
     , ((mod4Mask .|. shiftMask, xK_x), spawn "xkill")
     , ((mod4Mask .|. mod1Mask, xK_l), spawn "lxappearance")
     --, ((mod4Mask .|. shiftMask, xK_m), spawn "gnome-system-monitor")
     , ((mod4Mask .|. mod1Mask, xK_b), spawn "baobab")
     , ((mod4Mask .|. shiftMask, xK_v), spawn "virtualbox")
    -- Internet
     , ((mod4Mask .|. mod1Mask, xK_n), spawn "nm-connection-editor")
     , ((controlMask, xK_Escape), spawn "xfce4-popup-whiskermenu") 
     , ((mod1Mask, xK_Home), spawn "nautilus --no-desktop /home/igor")
  --   , ((mod1Mask, xK_g), spawn "geany")
  --   , ((mod1Mask, xK_x), spawn "gedit ~/.xmonad/xmonad.hs")
-- Переключение между запущенными программами

     --, ((mod1Mask, xK_F5     ), gotoMenu)
     --, ((mod1Mask, xK_F6     ), bringMenu)

    , ((mod4Mask .|. shiftMask, xK_w), spawn "wps")

-- Перезапуск Xmonad с перекомпиляцией

     --, ((mod4Mask  .|. shiftMask, xK_r), spawn "xmonad --recompile & xmonad --restart") restart "xmonad" True
     --, ((mod1Mask  .|. shiftMask, xK_r),  restart "xmonad" True)
     --, ((mod1Mask  .|. shiftMask, xK_r),  killAndRestart)
     , ((mod4Mask  .|. shiftMask, xK_r),  spawn "xmonad --recompile")
     , ((mod4Mask .|. shiftMask, xK_q), io (exitWith ExitSuccess))

--Volume

     , ((mod4Mask .|. shiftMask, xK_Up ), spawn "amixer -q sset Master 2+")
     , ((mod4Mask .|. shiftMask, xK_Down), spawn "amixer -q sset Master 2-")
     , ((mod4Mask .|. controlMask, xK_Up ), spawn "amixer -q sset Master 5+")
     , ((mod4Mask .|. controlMask, xK_Down), spawn "amixer -q sset Master 5-")

--mocp

     , ((0, 0x1008ff17), spawn "mocp -f")  
     , ((0, 0x1008ff16), spawn "mocp -r")  
     , ((0, 0x1008ff14), spawn "mocp -G")  
     , ((0, 0x1008ff15), spawn "mocp -s")

--deadbeef
     , ((mod4Mask, 0x1008ff14),         spawn "setsid deadbeef --play-pause")
     , ((mod4Mask, 0x1008ff17),         spawn "setsid deadbeef --next")
     , ((mod4Mask, 0x1008ff16),         spawn "setsid deadbeef --prev")

-- Shortcuts to open some sites with chromium-browser

     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_w), spawn "opera http://www.wikipedia.org/") 
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_y), spawn "opera http://www.youtube.com/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_g), spawn "opera http://www.google.com/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_m), spawn "opera http://www.mail.ru/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_k), spawn "opera http://www.vk.com/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_u), spawn "opera http://www.ukr.net/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_c), spawn "opera http://www.crestbook.com/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_z), spawn "opera http://www.yandex.ua/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_r), spawn "opera http://www.rutracker.org/")
     , ((mod4Mask .|. mod1Mask .|. controlMask, xK_t), spawn "opera http://www.gismeteo.ua/weather-kharkiv-5053/")

-- Multimedia keys

     , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 1-")		
     , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 1+")
     , ((0, xF86XK_Calculator), spawn "gnome-calculator") 


-- halt, reboot, suspend

     , ((mod4Mask .|. mod1Mask, xK_h), spawn "sudo shutdown -h now")
     , ((mod4Mask .|. mod1Mask, xK_r), spawn "sudo shutdown -r now")
     , ((mod4Mask .|. mod1Mask, xK_w), spawn "sudo pm-suspend")

--scratchpads

  , ((mod1Mask, xK_t), namedScratchpadAction scratchpads "goldendict") --V
  , ((mod1Mask, xK_m), namedScratchpadAction scratchpads "mocp") --
  , ((mod1Mask, xK_f), namedScratchpadAction scratchpads "mc") --V
  , ((mod1Mask, xK_n), namedScratchpadAction scratchpads "nitrogen") --V
  , ((mod1Mask, xK_r), namedScratchpadAction scratchpads "ranger") --V
  , ((mod1Mask, xK_c), namedScratchpadAction scratchpads "speedcrunch") --V
  , ((mod1Mask, xK_d), namedScratchpadAction scratchpads "dolphin") --V
  , ((mod1Mask, xK_h ), namedScratchpadAction scratchpads "htop") --V
  , ((mod1Mask, xK_l ), namedScratchpadAction scratchpads "lynx") --V
  , ((mod1Mask, xK_p ), namedScratchpadAction scratchpads "python") --No
  , ((mod1Mask, xK_a ), namedScratchpadAction scratchpads "alsamixer") --V  
  , ((mod1Mask, xK_v), namedScratchpadAction scratchpads "vim") --
  , ((mod1Mask, xK_z ), namedScratchpadAction scratchpads "cherrytree")
  , ((mod4Mask, xK_i ), namedScratchpadAction scratchpads "qtcreator1")
  , ((mod4Mask, xK_s), namedScratchpadAction scratchpads "gnome-search-tool")
  , ((mod4Mask, xK_F5 ), namedScratchpadAction scratchpads "terminal1")
  , ((mod4Mask, xK_F6 ), namedScratchpadAction scratchpads "terminal2")
  , ((mod4Mask, xK_F7 ), namedScratchpadAction scratchpads "terminal3")
  , ((mod4Mask, xK_F12), namedScratchpadAction scratchpads "transmission-gtk")
  --, ((mod1Mask, xK_p), namedScratchpadAction scratchpads "alpine") 
  -- Tags
   , ((mod1Mask, xK_w), withFocused (addTag "work"))
   , ((controlMask, xK_w), withFocused (delTag "work"))
   , ((mod1Mask .|. controlMask .|. shiftMask, xK_w), withTaggedP "work" (W.shiftWin "-:add1"))
   , ((mod4Mask .|. controlMask .|. shiftMask , xK_w), withTaggedGlobalP "work" shiftHere)
   , ((mod1Mask, xK_space), focusUpTaggedGlobal "work")
    ]
    ++
    [((m .|. mod4Mask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_Escape, xK_F1, xK_F2, xK_F3, xK_F4]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]

-- Mouse bindings

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((mod4Mask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- set the window to floating mode and move by dragging
    , ((mod4Mask, button2), (\w -> focus w >> windows W.shiftMaster)) -- raise the window to the top of the stack
    , ((mod4Mask, button3), (\w -> focus w >> FlexR.mouseResizeWindow w)) -- set the window to floating mode and resize by dragging
    , ((mod4Mask .|. shiftMask, button1), (\_ -> prevWS)) -- switch to previous workspace
    , ((mod4Mask .|. shiftMask, button3), (\_ -> nextWS)) -- switch to next workspace
    , ((mod4Mask .|. shiftMask, button4), (\_ -> sendMessage NextLayout))
    , ((mod4Mask, button4 ), (\_ -> spawn "amixer -q sset Master 1+"))
    , ((mod4Mask, button5 ), (\_ -> spawn "amixer -q sset Master 1-"))

    ]
-- window rules

myManageHook = (composeAll . concat $
   [
     [isDialog --> doFloat],
     [ (className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo  "F1:\xf03e"     | x <- images       ],
     [ (className =? v <||> title =? v <||> resource =? v) --> doShiftAndGo  "F2:\xf008"     | v <- videos       ],
     [ (className =? v <||> title =? v <||> resource =? v) --> doShiftAndGo  "F3:\xf025"     | v <- music        ],
     [ (className =? d <||> title =? d <||> resource =? d) --> doShiftAndGo  "3:\xf02d"      | d <- docs         ],
     [ (className =? d <||> title =? d <||> resource =? d) --> doShiftAndGo  "4:\xf044"      | d <- texts        ],
     [ (className =? d <||> title =? d <||> resource =? d) --> doShiftAndGo  "5:prg"         | d <- code         ],
     [ (className =? o <||> title =? o <||> resource =? o) --> doShiftAndGo  "8:\xf0e0"      | o <- mails        ],
     [ (className =? s <||> title =? s <||> resource =? s) --> doShiftAndGo  "7:\xf085"      | s <- systs        ],
     [ (className =? b <||> title =? b <||> resource =? b) --> doShiftAndGo  "3:\xf02d"      | b <- books        ],
     [ (className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo  "2:\xf0ac"      | x <- webs         ],
     [ (className =? w <||> title =? w <||> resource =? w) --> doShiftAndGo  "6:\xf0e3"      | w <- mathematic   ],
     [ (className =? w <||> title =? w <||> resource =? w) --> doShiftAndGo  "F1:\xf03e"     | w <- cads         ],
     [ (className =? m <||> title =? m <||> resource =? m) --> doShiftAndGo   "\xf115"       | m <- fileManagers ],
     [ (className =? g <||> title =? g <||> resource =? g) --> moveTo        "F1:\xf03e"     | g <- graphics     ],
     [ (className =? a <||> title =? a <||> resource =? a) --> doShiftAndGo  "6:\xf0e3"      | a <- games        ],
     [ (className =? a <||> title =? a <||> resource =? a) --> doShiftAndGo  "F4:\xf17e"     | a <- chats        ],
     [ (className =? a <||> title =? a <||> resource =? a) --> doShiftAndGo  "esc:\xf120"    | a <- terminals    ],
     [ (className =? a <||> title =? a <||> resource =? a) --> doIgnore                      | a <- ignores      ],
     [ (className =? a <||> title =? a <||> resource =? a) --> doCenterFloat                 | a <- myFloats     ],
     [(className =? "chromium-browser" <&&> resource =? "Dialog") --> doFloat],
     [ 
       appName =? "libreoffice" --> doShiftAndGo "3:\xf02d",
       appName =? "File Operation Progress" --> doCenterFloat,
       appName =? "Confirm to replace files" --> doCenterFloat,
       appName =? "Open" --> doCenterFloat,
       appName =? "Save" --> doCenterFloat,
       appName =? "Открыть" --> doCenterFloat,
       appName =? "Сохранить" --> doCenterFloat,
       className =? "VirtualBox" --> doShiftAndGo  "1:\xf17a",
       className =? "Terminator" --> doShiftAndGo "esc:\xf120",
       className =? "Krusader" --> doShiftAndGo "`:\xf115",
       className =? "Sgtimer" --> doCenterFloat,
       className =? "Java" --> doCenterFloat, 
       className =? "java" --> doCenterFloat,
       className =? "XClock" --> doCenterFloat,
       className =? "java-lang-Thread" --> doCenterFloat,
       className =? "sun-awt-X11-XFramePeer" --> doCenterFloat
       --className =? "QtCreator" --> doCenterFloat,
       --className =? "qtcreator" --> doCenterFloat,
       --className =? "jetbrains-idea-ce" --> doCenterFloat,
       --className =? "NetBeans IDE 8.2" --> doCenterFloat,
       --className =? "Eclipse" --> doCenterFloat,
       --className =? "Codeblocks" --> doCenterFloat
       --resource  =? "Conky" --> doIgnore 
 
     ] 
  ]) <+> namedScratchpadManageHook scratchpads

  where
            doShiftAndGo    = doF . liftM2 (.) W.greedyView W.shift
            moveTo          = doF . W.shift
            images          = ["Eog" , "Cheese", "Gpicview", "Shotwell","Comix", "Gwenview", "KOMPAS.Exe" ]
            videos          = ["Smplayer", "Vlc", "MPlayer", "Googleearth-bin", "mplayer2", "Kazam", "Minitube", "Smtube","smtube", "Kodi", "mpv", "gtk-youtube-viewer"]
            music           = ["Audacity", "Audacious", "Easytag", "Banshee", "Amarok", "Clementine", "Deadbeef", "Exaile", "Easytag"] 
            mathematic      = ["Wxmaxima", "Xmaxima", "Qtoctave", "Mathcad 14 Rus.exe"] --, "sun-awt-X11-XFramePeer", "java-lang-Thread"
            cads            = ["librecad", "DraftSight", "Bricscad", "Freecad", "KOMPASLT.Exe", "Qcad-bin"]
            docs            = ["tm", "pm", "pr", "Abiword", "Lyx", "pdfshuffler", "Pdfshuffler","Wps", "Wpp", "Et",
                                "libreoffice-startcenter", "libreoffice-writer", "libreoffice-math", "libreoffice-impress", 
                                "Gnumeric", "libreoffice-calc"]
            mails           = ["Thunderbird"]
            code            = ["Emacs24",  "Codeblocks", "Gvim", "Anjuta", "Krusader", "jetbrains-idea-ce", "Eclipse", "NetBeans IDE 8.2"]
            texts           = ["Gedit", "Medit", "Scribes", "Leafpad", "Kwrite", "Geany", "Sublime_text"]
            ignores         = ["Conky", "trayer", "Plasma"]
            chats           = ["Skype", "Qutim","Pidgin", "ViberPC", "Viber", "qTox", "TeamViewer", "utox","linphone", "Linphone"]
            fileManagers    = ["Spacefm"]
            systs           = ["Palimpsest","Gconf-editor", "Systemsettings", "Baobab", "Gnome-system-monitor","Bleachbit", "lxappearance", "Ubuntu-tweak", "kde-nm-connection-editor", "Gdebi-gtk"]
            books           = ["Evince", "Djview4", "Djview","Xchm","Chmsee", "Okular", "PdfMod", "zathura","Apvlv", "Xpdf" ]
            graphics        = ["Gimp", "Inkscape", "Dia-normal", "Pinta", "Kolourpaint", "Gimp-2.8", "Hotshots" ]
            games           = ["Knights", "XBoard", "Eboard", "Pychess", "Glchess", "dreamchess", "Dreamchess", "KasparovChess.exe" ]
            webs            = ["chromium-browser","Firefox", "Navigator", "Chromium-browser", "chromium-browser", "X-www-browser", "Yandex-browser", "Pale moon", "luakit", "Midori", "Rekonq", "Google-chrome",  "Opera", "Yandex-browser-beta", "Yandex-browser-beta (/home/igor/.config/yandex-browser-beta)"]
            terminals       = ["gnome-terminal", "Xterm"]
            myFloats        = ["Gimp", "Wrapper","File-roller", "file_properties", "Vidalia", "Nm-connection-editor", "Indicator-stickynotes.py", "Wrapper-1.0",  "Wrapper-1.0"]

-- layouts
myLayouts = maximize $ smartBorders $ onWorkspaces smpls smpLayout $
              onWorkspaces works workLayout $
              onWorkspaces ["\xf115"] filesLayout $ 
              onWorkspaces ["F1:\xf03e"] smpImgLayout $ 
              onWorkspaces ["F2:\xf008"] smpMediaLayout $
              onWorkspaces ["F3:\xf025"] smpLayout $
              onWorkspaces ["F4:\xf17e"] smpChatLayout $
              onWorkspaces ["1:\xf17a"] vboxLayout $
              onWorkspaces terms termsLayout $
              myLdefault 

smpls = ["7:\xf085","2:\xf0ac"] 
works = ["3:\xf02d", "5:prg"]
terms = ["esc:\xf120"]
myFull = named "myFull" (noBorders Full)
myTabs = named "myTabs" (gaps [(D,33)] $ simpleTabbed)
myTabs2 = named "myTabs2" $ (gaps [(D,33)] $ tabbed shrinkText tabConfig)
myCross = named "myCross" (gaps [(D,33)] $ simpleCross)
myCircle = named "myCircle" (gaps [(D,33)] $ Circle)
myRoledex = named "myRoledex" (gaps [(D,33)] $ Roledex)
myDishes = named "myDishes" (gaps [(D,33)] $ Dishes 2 (1/6))
mySpiral = named "mySpiral" (gaps [(D,33)] $ spiral (6/7))  
myGrid = named "myGrid" (gaps [(D,33)] $ Grid)
mySpacedGrid = named "mySpacedGrid" (gaps [(D,33)] $ spacing 5 $ Grid)
myMosaic = named "myMosaic" (gaps [(D,33)] $ mosaic 2 [3,2])
myGoldRatioTall = named "myGoldRatioTall" (gaps [(D,33)] $ ResizableTall 1 (3/100) (0.618034) [])
myRatioTall = named "myRatioTall" (gaps [(D,33)] $ ResizableTall 1 (3/100) (0.5) [])
myThreeCol = named "myThreeCol" (gaps [(D,33)] $ ThreeCol 1 (3/100) (1/3))
myThreeColMid = named "myThreeColMid" (gaps [(D,33)] $ ThreeColMid 1 (3/100) (1/2))
mySpacingGoldRatioTall = named "mySpacingGoldRatioTall" (gaps [(D,33)] $ smartSpacing 5 $ ResizableTall 1 (3/100) (0.618034) [])
myOneBig1 = named "myOneBig1" $ (gaps [(D,33)] $ windowNavigation ((Mirror $ Tall 1 (1/100) (3/4)) ***|* (Tall 0 (1/100) (1/2))))
myTabsAndFull = named "myTabsAndFull" $ (gaps [(D,33)] $ windowNavigation (simpleTabbed ****/*** (noBorders Full)))
myLayout3 = named "myLayout3" $ (gaps [(D,33)] $ windowNavigation ((TwoPane (3/100) (1/2))  **/* (noBorders Full)))
myDWMStyle = named "myDWMStyle" $ (gaps [(D,33)] $ dwmStyle shrinkText defaultTheme $ Tall 1 (3/100) (0.618034))
        --                              (tabbed shrinkText tabConfig)

tiled     = named "tiled" $ (gaps [(D,33)] $ smartBorders (ResizableTall 1 (2/100) (1/2) []))
reflectTiled = named "reflectTiled" $ (gaps [(D,33)] $ reflectHoriz tiled)

smpLayout = avoidStruts (myRatioTall ||| myTabs2 ||| myFull ||| myGrid)
termsLayout = avoidStruts (myRatioTall ||| myTabs2 ||| myGrid ||| myThreeCol ||| myThreeColMid ||| myFull)
myWebLayout = myTabs2 ||| myFull ||| myRatioTall
filesLayout = avoidStruts (myTabs2 ||| myFull ||| myRatioTall |||  myThreeColMid ||| myGrid ||| myRoledex ||| mosaic 2 [3,2] ||| mySpiral ||| simplestFloat)
workLayout = avoidStruts (myRatioTall ||| mySpacingGoldRatioTall ||| myTabs2 ||| myFull ||| simplestFloat ||| myThreeColMid ||| myLayout3 |||
                          myDishes ||| myTabsAndFull ||| myOneBig1 ||| myCross ||| myRoledex)

smpMediaLayout = myFull ||| myGoldRatioTall ||| myDWMStyle |||
                 myThreeColMid ||| myGrid |||
                 myCross ||| myTabs2
                
smpImgLayout = avoidStruts (myGoldRatioTall ||| myFull||| myThreeCol ||| myThreeColMid ||| 
               myGrid ||| myRoledex ||| myCross ||| myCircle ||| mySpiral ||| myTabs2 ||| myDWMStyle)

--Layouts_for_Skype------------------------------------------------------------------------------------------------

myIMLayout1 = withIM (1%7) skype Grid
    where
      skype = And (ClassName "Skype") (Role "") 

myIMLayout2 = smartBorders  $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout      = Grid
	        ratio = (1%9)
                skypeRatio = (1%8)
                skypeRoster  = (ClassName "Skype")     `And`
                               (Not (Title "Options")) `And`
                                              (Not (Role "Chats"))    `And`
                                                             (Not (Role "CallWindowForm"))
               
smpChatLayout = avoidStruts (myIMLayout1 ||| myIMLayout2 ||| mySpacingGoldRatioTall ||| mySpacedGrid |||  myFull)
------------------------------------------------------------------------------------------------------------------- 
vboxLayout = myFull ||| myTabs2
---------------------------------------------------------------------------------------------
--DEFAULT------------------------------------------------------------------------------------ 
myLdefault = avoidStruts (myRatioTall ||| myTabs2 ||| myFull ||| mySpacingGoldRatioTall |||
             myDWMStyle ||| (simpleDeco shrinkText myDeco $ Tall 1 (3/100) (0.618034)) |||
             myThreeCol ||| myThreeColMid |||  myGrid ||| myCircle ||| myCross ||| Accordion |||
             StackTile 1 (3/100) (1/2) ||| simplestFloat)
------------------------------------------------------------------------------------------------              
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
main :: IO ()
main = do
xmproc <- spawnPipe "/usr/bin/xmobar /home/igor/.xmonad/.xmobarrc"
xmonad $ ewmh defaultConfig
    { 
      manageHook =   manageDocks <+> myManageHook <+> manageHook defaultConfig <+> manageMenus <+> manageDialogs  
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , startupHook = spawn "~/.xmonad/startup.sh"
    --, startupHook = do 
    --  spawn "wmname LG3D"
    --  spawn "~/.xmonad/startup.sh"
    , modMask = myModMask
    , workspaces = myWorkspaces
    , focusedBorderColor = myFocusedBorderColor 
    , keys = myKeys
    , mouseBindings      = myMouseBindings
    , terminal = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , layoutHook = smartBorders  $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ mkToggle (single REFLECTX) $
      mkToggle (single REFLECTY) $ myLayouts
    , handleEventHook = do
        ewmhDesktopsEventHook
        --docksEventHook
        --fullscreenEventHook
        myEventHook
    , logHook = do 
        ewmhDesktopsLogHook 
        dynamicLogWithPP $ xmobarPP {
                 ppOutput = hPutStrLn xmproc
               , ppCurrent = xmobarColor "black" "#add8e6" . wrap "[" "]"
               , ppVisible = xmobarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
               , ppLayout  = wrap "" "" . xmobarColor "#ff0000" "" . wrap " " " " . (\ x -> case x of
                 "Maximize myFull"                       -> "[ ]"
                 "Maximize myGoldRatioTall"              -> "[GRT]"
                 "Maximize myRatioTall"                  -> "[MGRT]"
                 "Maximize myTabs2"                      -> "[--]"
                 "Maximize mySpacingGoldRatioTall"       -> "[SGRT]"
                 "Maximize myTabs"                       -> "[T]"
                 "Spacing  SimplestFloat"                -> "[F]"
                 "Maximize myThreeColMid"                -> "[3Col]"
                 "Maximize myLayout3"                    -> "[3]"
                 "Maximize myOneBig1"                    -> "[OneBig]"
                 "Maximize myTabsAndFull"                -> "[TF]"
                 "Spacing  myCross"                      -> "[S+]"
                 "Maximize  myCross"                     -> "[M+]"
                 "Maximize myGrid"                       -> "[G]"
                 "Maximize mySpacedGrid"                 -> "[SG]"
                 "Maximize myDWMStyle"                   -> "[OneBig]"
                 "Maximize myCircle"                     -> "[O]"
                 "Maximize Accordion"                    -> "[A]"
                 "Spacing  myMosaic"                     -> "[M]"
                 "Maximize myRoledex"                    -> "[R]" 
                 "Maximize myDishes"                     -> "[D]"  
                 "Maximize mySpiral"                     -> "[S]"
                 "Maximize Mosaic"                       -> "[M]"
                 "Maximize SimplestFloat"                -> "[MSF]"
                 "Maximize myWebLayout"                  -> "[W]"                   
                 _                                       -> x)  
               , ppTitle = xmobarColor "#00ff80" "" . shorten 50
               , ppHidden = xmobarColor "LightSkyBlue4" ""
               , ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByIndex
               , ppSep = ""
               , ppWsSep = " "
	}
    }
