import XMonad

import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile

import XMonad.Layout.Spacing
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.ShowWName
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
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
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions
import XMonad.Util.Run (spawnPipe)

import XMonad.Actions.MouseResize
import XMonad.Actions.NoBorders

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.ManageHook
import Data.Monoid
import Data.Char (toUpper)
import Graphics.X11.ExtraTypes.XF86 -- Epic keys
import System.Exit
import System.IO

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myWorkspaces = [ "1: \984515", "2: \58930", "3: \983609", "4: \984687", "5: \62601", "6: \61441", "7", "8", "9", "10"]
               -- At most, I use like 5 workspaces at a time I had no idea what to put for 7, 8, 9, or 10
myTerminal = "alacritty"

tall    = renamed [Replace "tall"]
        $ smartBorders
        $ windowNavigation
        $ subLayout [] (smartBorders Simplest)
        $ mySpacing 8
        $ ResizableTall 1 (3/100) (1/2) []
monocle = renamed [Replace "monocle"]
        $ noBorders
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

myShowWNameTheme = def
  { swn_font    = "xft:Ubuntu Nerd Font:bold:size=60"
  , swn_fade    = 1.0
  , swn_bgcolor = "#1c1f24"
  , swn_color   = "#ffffff"
  }

myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "ncmpcpp" spawnMus findMus manageMus
                , NS "calfw" spawnCal findCal manageCal
                ]
    where
      spawnTerm  = myTerminal ++ " -T scratchpad"
      findTerm   = title =? "scratchpad"
      manageTerm = customFloating $ W.RationalRect l t w h
                   where
                     h = 0.9
                     w = 0.9
                     t = 0.075 -- height based
                     l = 0.05 -- width based
      spawnMus  = myTerminal ++ " -T music -e ncmpcpp"
      findMus   = title =? "music"
      manageMus = customFloating $ W.RationalRect l t w h
                   where
                     h = 0.9
                     w = 0.9
                     t = 0.075 -- height based
                     l = 0.05 -- width based
      spawnCal  = "emacsclient -c -e '(cfw:open-org-calendar)' --title=cal"
      findCal   = resource =? "cal"
      manageCal = customFloating $ W.RationalRect l t w h
                   where
                     h = 0.9
                     w = 0.9
                     t = 0.075 -- height based
                     l = 0.05 -- width based

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "mpv /opt/sounds/startup-01.mp3"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnStatusBar "~/.config/polybar/launch.sh"
  spawnOnce "feh --randomize --bg-scale ~/.local/wallpapers"
  -- Makes repeat rate much faster
  spawnOnce "xset r rate 200 65"
  -- Epic caps lock instead of escape chad moment
  spawnOnce "setxkbmap -option caps:escape"
  -- This enables natural scrolling. Disable if scrolling direction feels weird for you
  spawnOnce "~/.config/xmonad/natScroll.sh"
  --compositor
  spawnOnce "picom"
  -- music
  spawnOnce "mpd"
  -- Emacs (no longer buggin)
  spawnOnce "emacs --daemon &"
  -- wifi
  spawnOnce "doas rfkill unblock wifi && iwctl station wlan0 scan"
  -- let java swing apps like intellij work
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
  , className =? "Yad"                                 --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  , isFullscreen                                       --> doFullFloat
  ] <+> manageDocks <+> namedScratchpadManageHook myScratchPads

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"JetBrains Mono\" --center --geometry=1200x800 --title \"XMonad keybindings\""
  hPutStr h  $ unlines $ showKm x
  hClose h
  return ()

myKeys c = let subKeys str ks = subtitle' str : mkNamedKeymap c ks in
        subKeys "Basic keybindings"
        [ ("M-S-<Return>", addName "Open a terminal"               $ windows W.focusMaster >> spawn myTerminal)
        , ("M-S-x",        addName "Close the current window"      $ kill)
        , ("M-p",          addName "Open the application launcher" $ spawn (concat ["rofi -show drun -terminal" , myTerminal]) >> spawn "mpv /opt/sounds/menu-01.mp3")
        , ("M-S-q",        addName "Exit XMonad"                   $ io (exitWith ExitSuccess) >> spawn "mpv /opt/sounds/shutdown-01.mp3" >> spawn "doas shutdown now")
        , ("M-S-r",        addName "Restart XMonad"                $ spawn "xmonad --recompile && xmonad --restart")
        ]

        ^++^ subKeys "Music"
        [ ("M-S-j",                  addName "Toggle the music"       $ spawn "mpc toggle")
        , ("<XF86AudioPlay>",        addName "Toggle the music"       $ spawn "mpc toggle")
        , ("M-S-h",                  addName "Play the previous song" $ spawn "mpc prev")
        , ("<XF86AudioPrev>",        addName "Play the previous song" $ spawn "mpc prev")
        , ("M-S-l",                  addName "Play the next song"     $ spawn "mpc next")
        , ("<XF86AudioNext>",        addName "Play the next song"     $ spawn "mpc next")
        , ("<XF86AudioRaiseVolume>", addName "Turn the volume up"     $ spawn "~/scripts/snd up")
        , ("<XF86AudioLowerVolume>", addName "Turn the volume down"   $ spawn "~/scripts/snd down")
        ]

        ^++^ subKeys "Brightness"
        [ ("<XF86MonBrightnessUp>",     addName "Turn the artificial brightness up"   $ spawn "real-brightness up")
        , ("<XF86MonBrightnessDown>",   addName "Turn the artificial brightness down" $ spawn "real-brightness down")

        , ("S-<XF86MonBrightnessUp>",   addName "Turn the brightness up"              $ spawn "brightness up")
        , ("S-<XF86MonBrightnessDown>", addName "Turn the brightness down"            $ spawn "brightness down")
        ]

        ^++^ subKeys "Scratchpads"
        [ ("M-s <Return>", addName "Open the terminal scratchpad" $ namedScratchpadAction myScratchPads "terminal")
        , ("M-s m",        addName "Open the music scratchpad"    $ namedScratchpadAction myScratchPads "ncmpcpp")
        , ("M-s c",        addName "Open the calendar scratchpad" $ namedScratchpadAction myScratchPads "calfw")
        ]

        ^++^ subKeys "Windows"
        [ ("M-j", addName "Go down the window stack"                   $ windows W.focusDown)
        , ("M-k", addName "Go up the window stack"                     $ windows W.focusUp)
        , ("M-h", addName "Shrink the master window"                   $ sendMessage Shrink)
        , ("M-l", addName "Expand the master window"                   $ sendMessage Expand)
        , ("M-<Return>", addName "Swap the current window with master" $ windows W.swapMaster)
        ]

        ^++^ subKeys "Workspaces"
        [ ("M-1", addName "Go to workspace 1"  $ ((windows $ W.greedyView $ myWorkspaces !! 0)))
        , ("M-2", addName "Go to workspace 2"  $ ((windows $ W.greedyView $ myWorkspaces !! 1)))
        , ("M-3", addName "Go to workspace 3"  $ ((windows $ W.greedyView $ myWorkspaces !! 2)))
        , ("M-4", addName "Go to workspace 4"  $ ((windows $ W.greedyView $ myWorkspaces !! 3)))
        , ("M-5", addName "Go to workspace 5"  $ ((windows $ W.greedyView $ myWorkspaces !! 4)))
        , ("M-6", addName "Go to workspace 6"  $ ((windows $ W.greedyView $ myWorkspaces !! 5)))
        , ("M-7", addName "Go to workspace 6"  $ ((windows $ W.greedyView $ myWorkspaces !! 6)))
        , ("M-8", addName "Go to workspace 8"  $ ((windows $ W.greedyView $ myWorkspaces !! 7)))
        , ("M-9", addName "Go to workspace 9"  $ ((windows $ W.greedyView $ myWorkspaces !! 8)))
        , ("M-0", addName "Go to workspace 10" $ ((windows $ W.greedyView $ myWorkspaces !! 9)))

        , ("M-S-1", addName "Send focused window to workspace 1"  $ ((windows $ W.shift $ myWorkspaces !! 0)))
        , ("M-S-2", addName "Send focused window to workspace 2"  $ ((windows $ W.shift $ myWorkspaces !! 1)))
        , ("M-S-3", addName "Send focused window to workspace 3"  $ ((windows $ W.shift $ myWorkspaces !! 2)))
        , ("M-S-4", addName "Send focused window to workspace 4"  $ ((windows $ W.shift $ myWorkspaces !! 3)))
        , ("M-S-5", addName "Send focused window to workspace 5"  $ ((windows $ W.shift $ myWorkspaces !! 4)))
        , ("M-S-6", addName "Send focused window to workspace 6"  $ ((windows $ W.shift $ myWorkspaces !! 5)))
        , ("M-S-7", addName "Send focused window to workspace 7"  $ ((windows $ W.shift $ myWorkspaces !! 6)))
        , ("M-S-8", addName "Send focused window to workspace 8"  $ ((windows $ W.shift $ myWorkspaces !! 7)))
        , ("M-S-9", addName "Send focused window to workspace 9"  $ ((windows $ W.shift $ myWorkspaces !! 8)))
        , ("M-S-0", addName "Send focused window to workspace 10" $ ((windows $ W.shift $ myWorkspaces !! 9)))

        , ("M-C-1", addName "Send focused window to workspace 1 and follow it"  $ ((windows (W.shift (myWorkspaces !! 0)))) >> ((windows $ W.greedyView $ myWorkspaces !! 0)))
        , ("M-C-2", addName "Send focused window to workspace 2 and follow it"  $ ((windows (W.shift (myWorkspaces !! 1)))) >> ((windows $ W.greedyView $ myWorkspaces !! 1)))
        , ("M-C-3", addName "Send focused window to workspace 3 and follow it"  $ ((windows (W.shift (myWorkspaces !! 2)))) >> ((windows $ W.greedyView $ myWorkspaces !! 2)))
        , ("M-C-4", addName "Send focused window to workspace 4 and follow it"  $ ((windows (W.shift (myWorkspaces !! 3)))) >> ((windows $ W.greedyView $ myWorkspaces !! 3)))
        , ("M-C-5", addName "Send focused window to workspace 5 and follow it"  $ ((windows (W.shift (myWorkspaces !! 4)))) >> ((windows $ W.greedyView $ myWorkspaces !! 4)))
        , ("M-C-6", addName "Send focused window to workspace 6 and follow it"  $ ((windows (W.shift (myWorkspaces !! 5)))) >> ((windows $ W.greedyView $ myWorkspaces !! 5)))
        , ("M-C-7", addName "Send focused window to workspace 7 and follow it"  $ ((windows (W.shift (myWorkspaces !! 6)))) >> ((windows $ W.greedyView $ myWorkspaces !! 6)))
        , ("M-C-8", addName "Send focused window to workspace 8 and follow it"  $ ((windows (W.shift (myWorkspaces !! 7)))) >> ((windows $ W.greedyView $ myWorkspaces !! 7)))
        , ("M-C-9", addName "Send focused window to workspace 9 and follow it"  $ ((windows (W.shift (myWorkspaces !! 8)))) >> ((windows $ W.greedyView $ myWorkspaces !! 8)))
        , ("M-C-0", addName "Send focused window to workspace 10 and follow it" $ ((windows (W.shift (myWorkspaces !! 9)))) >> ((windows $ W.greedyView $ myWorkspaces !! 9)))
        ]

        ^++^ subKeys "Layouts"
        [ ("M-<Space>", addName "Switch to the next layout"        $ sendMessage NextLayout)
        , ("M-t",       addName "Force a floating window to tile"  $ withFocused $ windows . W.sink)
        , ("M-m",       addName "Toggle monocle (fullscreen) mode" $ sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-f",       addName "Toggle a floating window"         $ sendMessage $ T.Toggle "floats")
        , ("M-b",       addName "Toggle polybar"                   $ sendMessage ToggleStruts >> spawn "polybar-msg cmd toggle")
        , ("M-S-b",     addName "Toggle the spacing for polybar"   $ sendMessage ToggleStruts)
        ]

        ^++^ subKeys "Misc"
        [ ("M-S-s s",   addName "Take a screenshot of part of the screen" $ unGrab *> spawn "import ~/Pictures/$(date +%Y%m%d_%H\\h%m\\m%Ss).png")
        , ("M-S-s S-s", addName "Take a screenshot of the whole screen"   $ unGrab *> spawn "import -window root ~/Pictures/$(date +%Y%m%d_%H\\h%m\\m%Ss).png")
        , ("M-w",       addName "Set a random wallpaper"                  $ spawn "feh --bg-scale --randomize ~/.local/wallpapers")
        , ("M-e",       addName "Spawn emacs"                             $ spawn "emacsclient -a 'emacs' -c")
        , ("M-=",       addName "Increase window spacing"                 $ incWindowSpacing 2 *> incScreenSpacing 2)
        , ("M--",       addName "Decrease window spacing"                 $ decWindowSpacing 2 *> decScreenSpacing 2)
        ]

main :: IO ()
main = do
        xmonad $ addDescrKeys' ((mod4Mask, xK_F1), showKeybindings) myKeys $ ewmhFullscreen $ addEwmhWorkspaceSort (pure (filterOutWs [scratchpadWorkspaceTag])) $ ewmh . docks  $ def {
        terminal                  = myTerminal
        , focusFollowsMouse       = True
        , clickJustFocuses        = False
        , handleEventHook         = windowedFullscreenFixEventHook <> swallowEventHook (className =? "Alacritty") (return True)
        , modMask                 = mod4Mask
        , workspaces              = myWorkspaces
        , layoutHook              = showWName' myShowWNameTheme $ myLayoutHook
        , startupHook             = myStartupHook
        , manageHook              = myManageHook
        --, logHook                 = showWNameLogHook myShowWNameTheme
        }
