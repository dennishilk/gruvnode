import XMonad
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (spacingRaw, Border (..))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main =
  xmonad
    . docks
    . ewmhFullscreen
    . ewmh
    $ def
      { terminal = "kitty"
      , modMask = mod4Mask
      , borderWidth = 2
      , focusedBorderColor = "#d79921"
      , normalBorderColor = "#3c3836"
      , layoutHook = myLayout
      , manageHook = myManageHook <+> manageHook def
      , startupHook = myStartupHook
      }
      `additionalKeysP` myKeys

myLayout =
  smartBorders
    . avoidStruts
    $ mySpacing
      (Tall 1 (3 / 100) (1 / 2) ||| Mirror (Tall 1 (3 / 100) (1 / 2)) ||| Full)

mySpacing = spacingRaw True (Border 6 6 6 6) True (Border 6 6 6 6) True

myManageHook = composeAll [isFullscreen --> doFullFloat]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xmobar ~/.config/xmobar/xmobarrc"
  spawnOnce "picom --experimental-backends"
  spawnOnce "setxkbmap de"
  spawnOnce "nitrogen --restore"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-<Return>", spawn "kitty")
  , ("M-d", spawn "dmenu_run")
  , ("M-b", spawn "xdg-open https://duckduckgo.com")
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-q", io exitSuccess)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-<Space>", sendMessage NextLayout)
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
  , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
  ]
