import XMonad
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Dwindle
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86 -- Epic keys
import System.Exit
import System.IO

  -- Bar stuff
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"]

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = avoidStruts $
            tiled ||| Mirror tiled ||| Spiral L XMonad.Layout.Dwindle.CW (3/2) (11/10) ||| Full
            where
                tiled = mySpacing 6 $ ResizableTall nmaster delta ratio []
                nmaster = 1
                ratio = 1/2
                delta = 3/100

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
  spawnOnce "mpc pause"
  -- This may look funky, but my emacs config buggin so idk why but i can only get it to work like this
  spawnOnce "emacs -Q -l ~/.config/emacs/init.elc --daemon || emacs -Q -l ~/.config/emacs/init.el --daemon"
  spawnOnce "doas rfkill unblock wifi && iwctl station wlan0 scan"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
        -- launch a terminal
        [ ((modm .|. shiftMask, xK_Return), windows W.focusMaster >> spawn "alacritty")

        -- application launcher
        , ((modm, xK_p), spawn "rofi -show drun" >> spawn "mpv /opt/sounds/menu-01.mp3")

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
        , ((0, xF86XK_MonBrightnessUp), spawn "brightness up")
        , ((0, xF86XK_MonBrightnessDown), spawn "brightness down")

        -- Change the background
        , ((modm, xK_w), spawn "feh --bg-scale --randomize ~/wallpapers")

        -- Moving around windows
        , ((modm, xK_j), windows W.focusDown)
        , ((modm, xK_k), windows W.focusUp)
        , ((modm, xK_h), sendMessage Shrink)
        , ((modm, xK_l), sendMessage Expand)
        , ((modm, xK_Return), windows W.swapMaster)
        , ((modm, xK_m), windows W.focusMaster)

        -- Exit XMonad
        , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess) >> spawn "mpv /opt/sounds/shutdown-01.mp3" >> spawn "doas shutdown now")
        -- Restart XMonad
        , ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")

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

        -- emacs
        , ((modm,  xK_e), spawn "emacsclient -a 'emacs' -c")

        -- manage window spacing
        , ((modm, xK_minus), decWindowSpacing 2 *> decScreenSpacing 2)
        , ((modm, xK_equal), incWindowSpacing 2 *> incScreenSpacing 2)
        ]

main :: IO ()
main = do
        --xmonad $ ewmhFullscreen $ ewmh $ def {
        xmonad $ ewmhFullscreen $ docks . ewmh $ def {
        terminal                = "urxvtc",
        focusFollowsMouse       = True,
        clickJustFocuses        = False,
        borderWidth             = 2,
        modMask                 = mod4Mask,
        workspaces              = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"],
        keys                    = myKeys,
        --eventHook               = ewmhDesktopsEventHook,

        layoutHook = myLayout,
        startupHook = myStartupHook
        }
