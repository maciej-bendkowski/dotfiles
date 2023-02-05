{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

import Data.Map as Map (Map, fromList)

import Control.Monad (forM, join, liftM2)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Graphics.X11.ExtraTypes (
  xF86XK_AudioLowerVolume,
  xF86XK_AudioMute,
  xF86XK_AudioNext,
  xF86XK_AudioPlay,
  xF86XK_AudioPrev,
  xF86XK_AudioRaiseVolume,
 )
import System.Exit (exitSuccess)
import XMonad
import XMonad.Hooks.DynamicLog (
  PP (ppCurrent),
  statusBar,
  wrap,
  xmobarColor,
  xmobarPP,
 )
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.StackSet (
  allWindows,
  focusDown,
  focusUp,
  focusWindow,
  greedyView,
  shift,
  swapDown,
  swapMaster,
  swapUp,
 )
import XMonad.Util.SpawnOnce (spawnOnce)

myKeybindings :: XConfig l -> Map (KeyMask, KeySym) (X ())
myKeybindings conf@(XConfig{modMask}) =
  Map.fromList $
    [ -- start a new terminal

      ( (modMask .|. shiftMask, xK_Return)
      , spawn $ terminal conf
      )
    , -- span the shell prompt

      ( (modMask, xK_p)
      , shellPrompt def
      )
    , -- cycle through layouts

      ( (modMask, xK_space)
      , sendMessage NextLayout
      )
    , -- next window

      ( (modMask, xK_Tab)
      , windows focusDown
      )
    , -- kill window
      ((modMask .|. shiftMask, xK_c), kill)
    , -- move focus around
      ((modMask, xK_j), windows focusDown)
    , ((modMask, xK_k), windows focusUp)
    , -- move windows around
      ((modMask .|. shiftMask, xK_j), windows swapDown)
    , ((modMask .|. shiftMask, xK_k), windows swapUp)
    , -- make current window the main one
      ((modMask, xK_Return), windows swapMaster)
    , -- increase / shrink master area
      ((modMask, xK_h), sendMessage Shrink)
    , ((modMask, xK_l), sendMessage Expand)
    , -- recompile and restart Xmonad

      ( (modMask .|. shiftMask, xK_r)
      , spawn "xmonad --recompile && xmonad --restart"
      )
    , -- quit xmonad
      ((modMask .|. shiftMask, xK_q), io exitSuccess)
    , -- system shutdown
      ((modMask .|. shiftMask, xK_Delete), spawn "sudo systemctl poweroff")
    , -- system lock
      ((modMask .|. shiftMask, xK_l), spawn "betterlockscreen --lock blur")
    ,
      ( (0, xF86XK_AudioMute)
      , spawn "toogle-sound"
      )
    , -- decrease volume

      ( (0, xF86XK_AudioLowerVolume)
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
      )
    , -- increase volume

      ( (0, xF86XK_AudioRaiseVolume)
      , spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
      )
    ,
      ( (modMask .|. shiftMask, xK_b)
      , spawn "toogle-headset"
      )
    , -- do not disturb mode for dunst

      ( (modMask .|. shiftMask, xK_d)
      , spawn "toogle-dunst"
      )
    , -- convenience

      ( (modMask .|. shiftMask, xK_p)
      , windows (greedyView "1:term")
      )
    ,
      ( (modMask .|. shiftMask, xK_bracketleft)
      , spawnFocus "Emacs" "emacsclient -c -a ''"
      )
    ,
      ( (modMask .|. shiftMask, xK_bracketright)
      , spawnFocus "Google-chrome" "google-chrome-stable"
      )
    ,
      ( (modMask .|. shiftMask, xK_backslash)
      , spawnFocus "Evince" "evince"
      )
    ,
      ( (modMask .|. shiftMask, xK_apostrophe)
      , spawnFocus "Slack" "slack"
      )
    ,
      ( (modMask .|. shiftMask, xK_semicolon)
      , spawnFocus "Discord" "discord"
      )
    ,
      ( (modMask .|. shiftMask, xK_slash)
      , spawnFocus "Spotify" "spotify"
      )
    ,
      ( (modMask, xK_Print)
      , spawn "gnome-screenshot -a"
      )
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    ]
      <> [ ((m .|. modMask, k), windows (f i))
         | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]
         , (f, m) <- [(greedyView, 0), (shift, shiftMask)]
         ]

-- | See, xprop | grep WM_CLASS
myManageHooks =
  composeAll
    [ className =? "Google-chrome" --> viewShift "3:web"
    , className =? "Emacs" --> viewShift "2:code"
    , className =? "Evince" --> viewShift "4:pdf"
    , className =? "Slack" --> viewShift "6:chat"
    , className =? "Discord" --> viewShift "6:chat"
    , className =? "" --> viewShift "7:spotify"
    ]
 where
  viewShift = doF . liftM2 (.) greedyView shift

myWorkspaces :: [String]
myWorkspaces =
  ["1:term", "2:code", "3:web", "4:pdf", "5:media", "6:chat", "7:spotify"]
    <> map show [8 .. 9]

myConfig =
  ewmh $
    def
      { modMask = mod4Mask
      , terminal = "alacritty"
      , normalBorderColor = "#7c7c7c"
      , focusedBorderColor = "#ffb6b0"
      , workspaces = myWorkspaces
      , manageHook = myManageHooks
      , layoutHook = myLayout
      , keys = myKeybindings
      , startupHook = do
          spawnOnce "kmonad ~/.config/kmonad.kbd"

          spawn "exec ~/.local/bin/lock.sh"
          spawnOnce "picom &"

          spawnOnce
            "trayer --edge top --align right --widthtype request \
            \--padding 6 --SetDockType true --SetPartialStrut true \
            \--expand true --transparent true --alpha 0 --iconspacing 2 \
            \--tint 0x2D2A2E --height 22 &"

          spawnOnce "feh --randomize --bg-fill ~/wallpapers/*"
          spawnOnce "nm-applet --sm-disable &"
          spawnOnce "blueman-applet &"

          spawnOnce "/usr/bin/emacs --daemon &"
      }
 where
  myLayout = smartBorders $ avoidStruts (mySpacing 6 defaultLayout)
  defaultLayout = Tall 1 (3 / 100) (1 / 2) ||| noBorders (fullscreenFull Full)
  mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

main :: IO ()
main = xmonad =<< statusBar "xmobar" myPP toogleStrutsKey myConfig
 where
  myPP :: PP
  myPP = xmobarPP{ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"}

  toogleStrutsKey :: XConfig l -> (KeyMask, KeySym)
  toogleStrutsKey XConfig{modMask} =
    (modMask, xK_b)

-- * Utilities
findWindows :: String -> X [Window]
findWindows name = do
  withWindowSet $ \ws -> do
    forM (allWindows ws) getWindow <&> join
 where
  getWindow w = do
    s <- withDisplay $ \d -> fmap resClass . liftIO $ getClassHint d w
    pure $ bool [] [w] (s == name) :: X [Window]

{- | Focuses the window with the given name or spawns a
   new instance using the given CLI command.
-}
spawnFocus :: String -> String -> X ()
spawnFocus name cmd =
  findWindows name >>= \case
    [] -> spawn cmd
    (w : _) -> windows $ focusWindow w
