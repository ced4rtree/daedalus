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

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]
myTerminal = "alacritty"

tall    = renamed [Replace "tall"]
        $ smartBorders
        $ windowNavigation
        -- $ subLayout [] (smartBorders Simplest) <- this causes issues
        $ mySpacing 8
        $ Tall 1 (3/100) (1/2)
monocle = renamed [Replace "monocle"]
        $ noBorders
        $ windowNavigation
        -- $ subLayout [] (smartBorders Simplest)
        $ Full
floats  = renamed [Replace "floats"]
        -- $ smartBorders
        $ simplestFloat

myLayoutHook = avoidStruts
               $ mouseResize
               $ windowArrange
               $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
  where
    myDefaultLayout = withBorder 1 tall
                               ||| noBorders monocle
                               ||| floats

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "autostart.sh" -- autostart shell file
  --spawnOnce "xmobar &"
  setWMName "LG3D" -- tricks programs into thining this is LG3D, which is the only thing java can work with for some reason

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

myKeys =
        -- launch a terminal
        [ ("M-S-<Return>", windows W.focusMaster >> spawn myTerminal)

        -- Close the focused window
        , ("M-S-x", kill)

        -- application launcher
        , ("M-p", spawn ("rofi -show drun -terminal" ++ myTerminal) >> spawn "mpv /opt/sounds/menu-01.mp3")

        -- Exit XMonad
        , ("M-S-q", io exitSuccess >> spawn "mpv /opt/sounds/shutdown-01.mp3" >> spawn "doas shutdown now")
        -- Restart XMonad
        , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")

        -- music control
        , ("M-S-j",                  spawn "emacsclient --eval '(emms-pause)' || mpc toggle")
        , ("<XF86AudioPlay>",        spawn "emacsclient --eval '(emms-pause)' || mpc toggle")
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
        , ("M-<Return>", windows W.swapMaster)

        , ("M-1", windows $ W.view $ head myWorkspaces)
        , ("M-2", windows $ W.view $ myWorkspaces !! 1)
        , ("M-3", windows $ W.view $ myWorkspaces !! 2)
        , ("M-4", windows $ W.view $ myWorkspaces !! 3)
        , ("M-5", windows $ W.view $ myWorkspaces !! 4)
        , ("M-6", windows $ W.view $ myWorkspaces !! 5)
        , ("M-7", windows $ W.view $ myWorkspaces !! 6)
        , ("M-8", windows $ W.view $ myWorkspaces !! 7)
        , ("M-9", windows $ W.view $ myWorkspaces !! 8)
        , ("M-0", windows $ W.view $ myWorkspaces !! 9)

        , ("M-S-1", windows $ W.shift $ head myWorkspaces)
        , ("M-S-2", windows $ W.shift $ myWorkspaces !! 1)
        , ("M-S-3", windows $ W.shift $ myWorkspaces !! 2)
        , ("M-S-4", windows $ W.shift $ myWorkspaces !! 3)
        , ("M-S-5", windows $ W.shift $ myWorkspaces !! 4)
        , ("M-S-6", windows $ W.shift $ myWorkspaces !! 5)
        , ("M-S-7", windows $ W.shift $ myWorkspaces !! 6)
        , ("M-S-8", windows $ W.shift $ myWorkspaces !! 7)
        , ("M-S-9", windows $ W.shift $ myWorkspaces !! 8)
        , ("M-S-0", windows $ W.shift $ myWorkspaces !! 9)

        , ("M-C-1", windows (W.shift (head myWorkspaces)) >> windows (W.view $ head myWorkspaces))
        , ("M-C-2", windows (W.shift (myWorkspaces !! 1)) >> windows (W.view $ myWorkspaces !! 1))
        , ("M-C-3", windows (W.shift (myWorkspaces !! 2)) >> windows (W.view $ myWorkspaces !! 2))
        , ("M-C-4", windows (W.shift (myWorkspaces !! 3)) >> windows (W.view $ myWorkspaces !! 3))
        , ("M-C-5", windows (W.shift (myWorkspaces !! 4)) >> windows (W.view $ myWorkspaces !! 4))
        , ("M-C-6", windows (W.shift (myWorkspaces !! 5)) >> windows (W.view $ myWorkspaces !! 5))
        , ("M-C-7", windows (W.shift (myWorkspaces !! 6)) >> windows (W.view $ myWorkspaces !! 6))
        , ("M-C-8", windows (W.shift (myWorkspaces !! 7)) >> windows (W.view $ myWorkspaces !! 7))
        , ("M-C-9", windows (W.shift (myWorkspaces !! 8)) >> windows (W.view $ myWorkspaces !! 8))
        , ("M-C-0", windows (W.shift (myWorkspaces !! 9)) >> windows (W.view $ myWorkspaces !! 9))

        -- Scroll through the layouts
        , ("M-<Space>", sendMessage NextLayout)
        -- Force a floating window back to tiling
        , ("M-t", withFocused $ windows . W.sink)
        -- Toggle fullscreen
        , ("M-m", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- >> spawn "polybar-msg cmd toggle")
        -- Toggle floating
        , ("M-f", sendMessage $ T.Toggle "floats")
        -- Toggle bar
        , ("M-b", sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
        -- Spacing can be pretty goofy sometimes, so here's just a keybinding exclusively for struts
        , ("M-S-b", sendMessage ToggleStruts)

        -- Screenshot
        , ("M-S-s s", unGrab *> spawn "import ~/Pictures/$(date +%Y%m%d_%H\\h%m\\m%Ss).png")
        , ("M-S-s S-s", unGrab *> spawn "import -window root ~/Pictures/$(date +%Y%m%d_%H\\h%m\\m%Ss).png")

        -- change background
        , ("M-w", spawn "feh --bg-scale --randomize ~/.local/wallpapers")

        -- emacs
        , ("M-e", spawn "emacsclient -a 'emacs' -c")

        -- manage window spacing
        , ("M--", decWindowSpacing 2 *> decScreenSpacing 2)
        , ("M-=", incWindowSpacing 2 *> incScreenSpacing 2)

        -- Copy Window functionality
        , ("M-v", windows copyToAll) -- make window always visible
        , ("M-S-v", killAllOtherCopies) -- return to ordinary state
        ]

main :: IO ()
main = do
        xmonad $ ewmhFullscreen $ docks . ewmh $ xmobarProp $ def {
        terminal                  = myTerminal
        , focusFollowsMouse       = True
        , clickJustFocuses        = False
        , handleEventHook         = windowedFullscreenFixEventHook <> swallowEventHook (className =? myTerminal) (return True)
        , modMask                 = mod4Mask
        , workspaces              = myWorkspaces
        , keys                    = (`mkKeymap` myKeys)
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , manageHook = myManageHook
        }
