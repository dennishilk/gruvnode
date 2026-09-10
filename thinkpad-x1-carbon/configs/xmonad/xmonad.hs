import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isDialog, isFullscreen)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Spacing (spacing)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

myTerminal :: String
myTerminal = "kitty"

myLayout =
  avoidStruts
    $ smartBorders
    $ spacing 6 (Tall 1 (3 / 100) (1 / 2))
      ||| noBorders Full

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ isFullscreen --> doFullFloat
    , isDialog --> doCenterFloat
    ]

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --no-fehbg --bg-fill \"$HOME/.local/share/wallpapers/gruvnode-background.png\""
  spawnOnce "picom --config \"$HOME/.config/picom/picom.conf\""
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"

myKeys :: [(String, X ())]
myKeys =
  [ ("M-<Return>", spawn myTerminal)
  , ("M-d", spawn "rofi -show drun")
  , ("M-b", spawn "google-chrome-stable")
  , ("M-o", spawn "obs")
  , ("M-q", kill)
  , ("M-S-q", io exitSuccess)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-m", windows W.focusMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("<Print>", spawn "mkdir -p \"$HOME/Pictures/Screenshots\" && scrot \"$HOME/Pictures/Screenshots/%Y-%m-%d_%H-%M-%S.png\"")
  , ("<XF86AudioRaiseVolume>", spawn "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+")
  , ("<XF86AudioLowerVolume>", spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
  , ("<XF86AudioMute>", spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
  , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
  ]

main :: IO ()
main = do
  xmobarHandle <- spawnPipe "xmobar \"$HOME/.config/xmobar/xmobarrc\""

  let baseConfig =
        def
          { terminal = myTerminal
          , modMask = mod4Mask
          , borderWidth = 2
          , normalBorderColor = "#3c3836"
          , focusedBorderColor = "#b8bb26"
          , layoutHook = myLayout
          , manageHook = myManageHook <+> manageHook def
          , startupHook = myStartupHook
          , logHook =
              dynamicLogWithPP
                xmobarPP
                  { ppOutput = hPutStrLn xmobarHandle
                  , ppCurrent = xmobarColor "#b8bb26" "" . wrap "[" "]"
                  , ppVisible = xmobarColor "#83a598" ""
                  , ppHidden = xmobarColor "#ebdbb2" ""
                  , ppHiddenNoWindows = xmobarColor "#665c54" ""
                  , ppTitle = xmobarColor "#d5c4a1" "" . shorten 60
                  , ppSep = "  "
                  , ppWsSep = " "
                  }
          }

  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    $ baseConfig `additionalKeysP` myKeys
