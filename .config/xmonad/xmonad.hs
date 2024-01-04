import XMonad

import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Dwindle
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Grid
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.EwmhDesktops

-- Bar stuff
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.ClickableWorkspaces

import XMonad.Actions.MouseResize
import XMonad.Actions.NoBorders
import XMonad.Actions.CopyWindow

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86 -- Epic keys
import System.Exit
import System.IO

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myWorkspaces = [ " 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 ", " 10 "]
myTerminal = "urxvt"

myLayoutHook =
  toggleLayouts (noBorders Full) (smartBorders $ mySpacing 8 $ ResizableTall 1 (1/15) (6/10) [])
  ||| (smartBorders $
       mySpacing 8 $
        Grid
        ||| Dwindle R CW 1.5 1.1
        ||| Spiral R CW 1.5 1.1
        ||| Squeeze R 1.0 1.0)

  
myStartupHook :: X ()
myStartupHook = do
  -- sound
  spawnOnce "pipewire &"
  spawnOnce "pipewire-pulse &"
  spawnOnce "wireplumber &"
  spawnOnce "mpv /opt/sounds/startup-01.mp3 &"

  -- x settings
  spawn "xsetroot -cursor_name left_ptr"
  spawn "setxkbmap -option ctrl:nocaps"
  spawn "xset r rate 200 65"
  spawn "natScroll.sh"

  -- misc
  spawnOnce "xcompmgr &"
  spawnOnce "/usr/lib/polkit-kde-authentication-agent-1 &"
  spawnOnce "emacs --daemon &"
  spawnOnce "feh --bg-scale ~/.local/share/wallpapers/wallpaper.jpg"
  spawnOnce "batsignal -M 'dunstify' &"
  spawnOnce "mpd"

  spawnOnce "~/.config/xmonad/trayer.sh &"

  -- make java apps work
  setWMName "LG3D"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "confirm"                             --> doFloat
  , className =? "file_progress"                       --> doFloat
  , className =? "dialog"                              --> doFloat
  , className =? "download"                            --> doFloat
  , className =? "error"                               --> doFloat
  , className =? "Gimp"                                --> doFloat
  , className =? "notification"                        --> doFloat
  , className =? "splash"                              --> doFloat
  , className =? "toolbar"                             --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , isFullscreen                                       --> doFullFloat
  ] <+> manageDocks

-- myKeys :: XConfig -> List
myKeys conf =
        -- launch a terminal
        [ ("M-S-<Return>", windows W.focusMaster >> spawn myTerminal)

        -- Close the focused window
        , ("M-S-x", kill)

        -- application launcher
        , ("M-p", spawn ("rofi -show drun -terminal " ++ myTerminal) >> spawn "mpv /opt/sounds/menu-01.mp3")

        -- Exit XMonad
        , ("M-S-q", io exitSuccess)
        -- Restart XMonad
        , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")

        -- music control
        , ("M-S-j",                  spawn "mpc toggle")
        , ("<XF86AudioPlay>",        spawn "mpc toggle")
        , ("M-S-h",                  spawn "mpc prev")
        , ("<XF86AudioPrev>",        spawn "mpc prev")
        , ("M-S-l",                  spawn "mpc next")
        , ("<XF86AudioNext>",        spawn "mpc next")
        , ("<XF86AudioRaiseVolume>", spawn "snd up")
        , ("<XF86AudioLowerVolume>", spawn "snd down")

        -- Brightness adjustment
        , ("<XF86MonBrightnessUp>", spawn "real-brightness up")
        , ("<XF86MonBrightnessDown>", spawn "real-brightness down")

        , ("S-<XF86MonBrightnessUp>", spawn "brightness up")
        , ("S-<XF86MonBrightnessDown>", spawn "brightness down")

        -- Moving around windows
        , ("M-j", windows W.focusDown)
        , ("M-k", windows W.focusUp)
        , ("M-h", sendMessage Shrink)
        , ("M-l", sendMessage Expand)
        , ("M-z", sendMessage MirrorShrink)
        , ("M-a", sendMessage MirrorExpand)
        , ("M-<Return>", windows W.swapMaster)
        ] ++

        [ ("M-" ++ modKey2 ++ [keyChar], windows $ windowOperation workspaceId)
        | (workspaceId, keyChar) <- zip (workspaces conf) "123456789"
        , (windowOperation, modKey2) <- [(W.view, ""), (W.shift, "S-"), (W.view, "C-"), (W.shift, "C-")]
        ] ++

        -- Scroll through the layouts
        [ ("M-<Space>", sendMessage NextLayout)
        -- Force a floating window back to tiling
        , ("M-t", withFocused $ windows . W.sink)
        -- Toggle fullscreen
        , ("M-m", sendMessage (Toggle "Full") >> sendMessage ToggleStruts) -- >> spawn "polybar-msg cmd toggle")
        -- Toggle floating
        , ("M-f", sendMessage $ T.Toggle "floats")
        -- Toggle bar
        , ("M-b", sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
        -- Spacing can be pretty goofy sometimes, so here's just a keybinding exclusively for struts
        , ("M-S-b", sendMessage ToggleStruts)

        -- Screenshot
        , ("M-S-s s", unGrab *> spawn "import ~/Pictures/$(date +%Y%m%d_%H\\h%m\\m%Ss).png")
        , ("M-S-s S-s", unGrab *> spawn "import -window root ~/Pictures/$(date +%Y%m%d_%H\\h%m\\m%Ss).png")

        -- emacs
        , ("M-e", spawn "emacsclient -a 'emacs' -c")

        -- manage window spacing
        , ("M--", decWindowSpacing 2 *> decScreenSpacing 2)
        , ("M-=", incWindowSpacing 2 *> incScreenSpacing 2)

        -- Copy Window functionality
        , ("M-v", windows copyToAll) -- make window always visible
        , ("M-S-v", killAllOtherCopies) -- return to ordinary state

        -- logout
        , ("M-<Escape>", spawn "archlinux-logout")
        ]

-- Xmobar Config
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = " | "
    , ppCurrent         = white . xmobarBorder "VBoth" "#ff0000" 2
    , ppHidden          = lowWhite
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, layout, _, window] -> [ws, window, layout]
    , ppExtras          = [logTitles formatFocused (\w -> "")]
    }
  where
    formatFocused = wrap (red "[") (red "]") . white . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 45
    
    red, white, lowWhite , yellow :: String -> String
    red      = xmobarColor "#ff5555" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    lowWhite = xmobarColor "#666666" ""

myConfig = def
  { terminal                  = myTerminal
  , focusFollowsMouse       = True
  , clickJustFocuses        = False
  , handleEventHook         = windowedFullscreenFixEventHook <> swallowEventHook (className =? myTerminal) (return True)
  , modMask                 = mod4Mask
  , workspaces              = myWorkspaces
  -- , keys                    = myKeys
  , layoutHook              = myLayoutHook
  , startupHook             = myStartupHook
  , manageHook              = myManageHook
  }

myConfig' = myConfig `additionalKeysP` (myKeys myConfig)

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     . withEasySB (statusBarProp "xmobar" (clickablePP myXmobarPP)) defToggleStrutsKey
     $ myConfig'
