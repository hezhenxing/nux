import XMonad
import XMonad.Actions.Volume
import XMonad.Actions.ToggleFullFloat (toggleFullFloat)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.ManageHelpers (doFullFloat)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (spacingRaw, Border(Border))
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce (spawnOnce)

import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus
import qualified DBus.Client as DBus
import qualified XMonad.StackSet as W

import System.Exit (exitSuccess)
import Graphics.X11.ExtraTypes.XF86

myModMask = mod4Mask
myTerminal = "kitty"
myBrowser  = "goggle-chrome-stable"
myEditor   = "code"
myWallpaper = "~/wallpapers/wallpaper"


rofi_launcher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "
appLauncher  = "rofi -modi drun,ssh,window -show drun -show-icons"
calcLauncher = "rofi -show calc -modi calc -no-show-match -no-sort"
screenLocker = "betterlockscreen -l dim"
playerctl c  = "playerctl --player=spotify " <> c


main :: IO ()
main = mkDBusClient >>= main'

main' :: DBus.Client -> IO ()
main' dbus
  = xmonad
  $ ewmhFullscreen
  $ docks
  $ ewmh
  $ myConfig dbus

myLogHook = fadeInactiveLogHook 0.9

mkDBusClient :: IO DBus.Client
mkDBusClient = do
  dbus <- DBus.connectSession
  DBus.requestName dbus (DBus.busName_ "org.xmonad.log") opts
  return dbus
  where
    opts = [ DBus.nameAllowReplacement, DBus.nameReplaceExisting, DBus.nameDoNotQueue ]

dbusOutput :: DBus.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = DBus.objectPath_ "/org/xmonad/Log"
      iname  = DBus.interfaceName_ "org.xmonad.Log"
      mname  = DBus.memberName_ "Update"
      signal = DBus.signal opath iname mname
      body   = [ DBus.toVariant $ UTF8.decodeString str ]
  in  DBus.emit dbus $ signal { DBus.signalBody = body }

polybarHook :: DBus.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = wrapper purple . shorten 90
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

myConfig dbus = def
  { modMask         = myModMask
  , terminal        = myTerminal
  , layoutHook      = myLayout
  , logHook         = myPolybarLogHook dbus
  , startupHook     = myStartupHook
  , manageHook      = myManageHook
  }
  `additionalKeys`
  [ ((mod,           xK_c),                     kill)
  , ((mod,           xK_e),                     chrome)
  , ((mod,           xK_f),                     withFocused toggleFullFloat)
  , ((mod,           xK_equal),                 togglePolybar)
  , ((mod .|. shift, xK_l),                     spawn screenLocker)
  , ((mod,           xK_o),                     rofi_launcher)
  , ((mod,           xK_p),                     spawn appLauncher)
  , ((mod,           xK_q),                     quit)
  , ((mod,           xK_v),                     spawn "code")
  , ((mod,           xK_w),                     spawn "brave")
  , ((0,             xF86XK_AudioMute),         audioMute)
  , ((0,             xF86XK_AudioLowerVolume),  audioLower)
  , ((0,             xF86XK_AudioRaiseVolume),  audioRaise)
  , ((0,             xF86XK_MonBrightnessUp),   brightUp)
  , ((0,             xF86XK_MonBrightnessDown), brightDown)
  ]
  ++
  [((mod .|. controlMask, k), windows $ (W.greedyView i) . (W.shift i))
    | (i, k) <- zip (workspaces def) [xK_1 .. xK_9]
  ]
  where
    mod           = myModMask
    shift         = shiftMask
    quit          = io exitSuccess
    chrome        = spawn "google-chrome-stable"
    togglePolybar = spawn "polybar-msg cmd toggle"
    audioMute     = spawn "polybar-msg action \"#pipewire.hook.1\""
    audioLower    = spawn "polybar-msg action \"#pipewire.hook.2\""
    audioRaise    = spawn "polybar-msg action \"#pipewire.hook.3\""
    brightDown    = spawn "brightnessctl set 5%-"
    brightUp      = spawn "brightnessctl set +5%"

myLayout
  = gaps [(L,10), (R,10), (U,60), (D,10)]
  $ spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True
  $ smartBorders
  $ tiled ||| Mirror tiled ||| Full
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

myStartupHook = do
  -- FIXME: Have to wait for 1 second, or polybar will fail to initialize VOL
  spawnOnce "sleep 1 && dunst"
  spawnOnce "sleep 1 && polybar"

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Wlogout" --> doFullFloat ]

myXmobarPP :: PP
myXmobarPP = def
  { ppSep             = magenta " • "
  , ppTitleSanitize   = xmobarStrip
  , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
  , ppHidden          = white . wrap " " ""
  , ppHiddenNoWindows = lowWhite . wrap " " ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
  , ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""