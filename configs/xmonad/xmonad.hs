import XMonad
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)

import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, defToggleStrutsKey)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Hooks.StatusBar (StatusBarConfig, withEasySB, statusBarProp)
import XMonad.Hooks.StatusBar.PP (filterOutWsPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Util.EZConfig (additionalKeysP)

main :: IO ()
main =
  xmonad
    . withEasySB mySB defToggleStrutsKey
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

mySB :: StatusBarConfig
mySB = statusBarProp "xmobar -x 0 ~/.config/xmobar/xmobarrc" (pure myXmobarPP)

myXmobarPP =
  filterOutWsPP ["NSP"]
    xmobarPP
      { ppCurrent = xmobarColor "#fabd2f" "" . wrap "[" "]"
      , ppVisible = xmobarColor "#b8bb26" "" . wrap "(" ")"
      , ppHidden = xmobarColor "#ebdbb2" ""
      , ppHiddenNoWindows = xmobarColor "#7c6f64" ""
      , ppTitle = xmobarColor "#83a598" "" . shorten 80
      , ppSep = xmobarColor "#665c54" "" "  •  "
      }

myLayout =
  smartBorders
    . avoidStruts
    $ mySpacing (Tall 1 (3 / 100) (1 / 2) ||| Mirror (Tall 1 (3 / 100) (1 / 2)) ||| Full)

mySpacing = spacingRaw True (Border 6 6 6 6) True (Border 6 6 6 6) True

myManageHook = composeAll [isFullscreen --> doFullFloat]

myStartupHook :: X ()
myStartupHook = do
  spawn "setxkbmap de -option caps:escape"
  spawn "sh -c 'command -v feh >/dev/null && feh --bg-fill ~/.local/share/wallpapers/gruvnode.jpg'"
  spawn "sh -c 'command -v picom >/dev/null && pgrep -x picom >/dev/null || picom --config ~/.config/picom/picom.conf'"
  spawn "sh -c 'command -v nm-applet >/dev/null && pgrep -x nm-applet >/dev/null || nm-applet --indicator'"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-<Return>", spawn "kitty")
  , ("M-d", spawn "dmenu_run")
  , ("M-b", spawn "xdg-open https://duckduckgo.com")
  , ("M-S-c", spawn "xmonad --recompile")
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-q", io exitSuccess)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-S-s", spawn "scrot ~/Pictures/screenshot-%Y%m%d-%H%M%S.png")
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
  , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
  , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
  ]
