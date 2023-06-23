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
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
import XMonad.Actions.MouseResize
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86 -- Epic keys
import System.Exit
import System.IO

  -- Bar stuff
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myWorkspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

myTerminal = "alacritty"

tall    = renamed [Replace "tall"]
        $ smartBorders
        $ windowNavigation
        $ subLayout [] (smartBorders Simplest)
        $ mySpacing 8
        $ ResizableTall 1 (3/100) (1/2) []
monocle = renamed [Replace "monocle"]
        $ smartBorders
        $ windowNavigation
        $ subLayout [] (smartBorders Simplest)
        $ Full
floats  = renamed [Replace "floats"]
        $ smartBorders
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

--myLayout = avoidStruts $
            --toggleLayouts tiled $ noBorders Full
            --where
                --tiled = mySpacing 6 $ ResizableTall nmaster delta ratio []
                --nmaster = 1
                --ratio = 1/2
                --delta = 3/100

myHome = "/home/some-guy"

myStartupHook :: X ()
myStartupHook = do
  spawnOnce $ concat ["export PATH=${PATH}:", myHome, "/scripts"]
  spawnOnce "mpv /opt/sounds/startup-01.mp3"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawn $ concat [myHome, "/.config/polybar/launch.sh"]
  spawnOnce $ concat ["feh --randomize --bg-scale ", myHome, "/wallpapers"]
  -- Makes repeat rate much faster
  spawnOnce "xset r rate 200 65"
  -- Epic caps lock instead of escape chad moment
  spawnOnce "setxkbmap -option caps:escape"
  -- This enables natural scrolling. Disable if scrolling direction feels weird for you
  spawnOnce $ concat [ myHome, "/.config/xmonad/natScroll.sh" ]

  spawnOnce "mpd"

  -- Emacs (no longer buggin)
  spawnOnce "emacs --daemon"

  -- wifi
  spawnOnce "doas rfkill unblock wifi && iwctl station wlan0 scan"

  -- let java swing apps like intellij work
  setWMName "LG3D"

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "confirm"       --> doFloat
  , className =? "file_progress" --> doFloat
  , className =? "dialog"        --> doFloat
  , className =? "download"      --> doFloat
  , className =? "error"         --> doFloat
  , className =? "Gimp"          --> doFloat
  , className =? "notification"  --> doFloat
  , className =? "splash"        --> doFloat
  , className =? "toolbar"       --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , isFullscreen --> doFullFloat
  ] <+> manageDocks

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        -- launch a terminal
        [ ((modm .|. shiftMask, xK_Return), windows W.focusMaster >> spawn myTerminal)

        -- application launcher
        , ((modm, xK_p), spawn "rofi -show drun -terminal alacritty" >> spawn "mpv /opt/sounds/menu-01.mp3")

        -- Close the focused window
        , ((modm .|. shiftMask, xK_x), kill)

        -- music control
        , ((modm .|. shiftMask, xK_j), spawn "mpc toggle")
        , ((modm .|. shiftMask, xK_h), spawn "mpc prev")
        , ((modm .|. shiftMask, xK_l), spawn "mpc next")
        , ((0, xF86XK_AudioRaiseVolume), spawn "~/scripts/snd up")
        , ((0, xF86XK_AudioLowerVolume), spawn "~/scripts/snd down")

        -- Brightness adjustment
        -- , ((modm, xK_F10), spawn "~/scripts/brightness down")
        -- , ((modm, xK_F11), spawn "~/scripts/brightness up")
        , ((shiftMask, xF86XK_MonBrightnessUp), spawn "brightness up")
        , ((shiftMask, xF86XK_MonBrightnessDown), spawn "brightness down")

        , ((0, xF86XK_MonBrightnessUp), spawn "real-brightness up")
        , ((0, xF86XK_MonBrightnessDown), spawn "real-brightness down")

        -- Change the background
        , ((modm, xK_w), spawn "feh --bg-scale --randomize ~/wallpapers")

        -- Screenshot
        , ((modm, xK_s), unGrab *> spawn "scrot 'pictures/%Y-%m-%d-%H-%M.png' -s")

        -- Moving around windows
        , ((modm, xK_j), windows W.focusDown)
        , ((modm, xK_k), windows W.focusUp)
        , ((modm, xK_h), sendMessage Shrink)
        , ((modm, xK_l), sendMessage Expand)
        , ((modm, xK_Return), windows W.swapMaster)

        -- Exit XMonad
        , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess) >> spawn "mpv /opt/sounds/shutdown-01.mp3" >> spawn "doas shutdown now")
        -- Restart XMonad
        , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile && xmonad --restart")

        , ((modm, xK_1), ((windows $ W.greedyView $ myWorkspaces !! 0)))
        , ((modm, xK_2), ((windows $ W.greedyView $ myWorkspaces !! 1)))
        , ((modm, xK_3), ((windows $ W.greedyView $ myWorkspaces !! 2)))
        , ((modm, xK_4), ((windows $ W.greedyView $ myWorkspaces !! 3)))
        , ((modm, xK_5), ((windows $ W.greedyView $ myWorkspaces !! 4)))
        , ((modm, xK_6), ((windows $ W.greedyView $ myWorkspaces !! 5)))
        , ((modm, xK_7), ((windows $ W.greedyView $ myWorkspaces !! 6)))
        , ((modm, xK_8), ((windows $ W.greedyView $ myWorkspaces !! 7)))
        , ((modm, xK_9), ((windows $ W.greedyView $ myWorkspaces !! 8)))
        , ((modm, xK_0), ((windows $ W.greedyView $ myWorkspaces !! 9)))

        , ((modm .|. shiftMask, xK_1), ((windows $ W.shift $ myWorkspaces !! 0)))
        , ((modm .|. shiftMask, xK_2), ((windows $ W.shift $ myWorkspaces !! 1)))
        , ((modm .|. shiftMask, xK_3), ((windows $ W.shift $ myWorkspaces !! 2)))
        , ((modm .|. shiftMask, xK_4), ((windows $ W.shift $ myWorkspaces !! 3)))
        , ((modm .|. shiftMask, xK_5), ((windows $ W.shift $ myWorkspaces !! 4)))
        , ((modm .|. shiftMask, xK_6), ((windows $ W.shift $ myWorkspaces !! 5)))
        , ((modm .|. shiftMask, xK_7), ((windows $ W.shift $ myWorkspaces !! 6)))
        , ((modm .|. shiftMask, xK_8), ((windows $ W.shift $ myWorkspaces !! 7)))
        , ((modm .|. shiftMask, xK_9), ((windows $ W.shift $ myWorkspaces !! 8)))
        , ((modm .|. shiftMask, xK_0), ((windows $ W.shift $ myWorkspaces !! 9)))

        -- Scroll through the layouts
        , ((modm, xK_space), sendMessage NextLayout)
        -- Force a floating window back to tiling
        , ((modm, xK_t), withFocused $ windows . W.sink)
        -- Toggle fullscreen
        , ((modm, xK_m), sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
        -- Toggle floating
        , ((modm, xK_f), sendMessage $ T.Toggle "floats")
        -- Toggle bar
        , ((modm, xK_b), sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
        -- Spacing can be pretty goofy sometimes, so here's just a keybinding exclusively for struts
        , ((modm .|. shiftMask, xK_b), sendMessage ToggleStruts)

        -- emacs
        , ((modm,  xK_e), spawn "emacsclient -a 'emacs' -c")

        -- manage window spacing
        , ((modm, xK_minus), decWindowSpacing 2 *> decScreenSpacing 2)
        , ((modm, xK_equal), incWindowSpacing 2 *> incScreenSpacing 2)
        ]

main :: IO ()
main = do
        xmonad $ ewmhFullscreen $ docks . ewmh $ def {
        terminal                  = myTerminal
        , focusFollowsMouse       = True
        , clickJustFocuses        = False
        , borderWidth             = 1
        , modMask                 = mod4Mask
        , workspaces              = myWorkspaces
        , keys                    = myKeys
        , layoutHook = myLayoutHook
        , startupHook = myStartupHook
        , manageHook = myManageHook
        }
