------------------------------------------------------------
-- Gruvnode XMonad Config
-- Debian 13 / ThinkPad T480
------------------------------------------------------------

import XMonad
import qualified XMonad.StackSet as W
import System.Exit (exitSuccess)


-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

-- Utils
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)

------------------------------------------------------------
-- Main
------------------------------------------------------------
main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    $ def
        { terminal           = "kitty"
        , modMask            = mod4Mask
        , borderWidth        = 2
        , focusedBorderColor = "#89b4fa"
        , normalBorderColor  = "#1e1e2e"
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , startupHook        = myStartupHook
        }
        `additionalKeysP` myKeys

------------------------------------------------------------
-- Layout
------------------------------------------------------------
myLayout =
  smartBorders
    $ spacingWithEdge 6
    $ Tall 1 (3 / 100) (1 / 2)
      ||| Full

------------------------------------------------------------
-- ManageHook
------------------------------------------------------------
myManageHook =
  composeAll
    [ isFullscreen --> doFullFloat
    ]

------------------------------------------------------------
-- Startup
------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "feh --bg-fill ~/Pictures/wallpapers/1.png"
  setWMName "LG3D"   -- Wine / Proton compatibility

------------------------------------------------------------
-- Keybindings
------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
  [ -- Apps
    ("M-<Return>", spawn "kitty")
  , ("M-d",        spawn "dmenu_run")
  , ("M-b",        spawn "google-chrome-stable")

    -- Window
  , ("M-q",        kill)
  , ("M-S-q",      io exitSuccess)

    -- Layouts
  , ("M-<Space>",  sendMessage NextLayout)

    -- Focus
  , ("M-j",        windows W.focusDown)
  , ("M-k",        windows W.focusUp)
  , ("M-m",        windows W.focusMaster)

    -- Swap
  , ("M-S-j",      windows W.swapDown)
  , ("M-S-k",      windows W.swapUp)

    -- Resize
  , ("M-h",        sendMessage Shrink)
  , ("M-l",        sendMessage Expand)

    -- Reload XMonad
  , ("M-S-r",      spawn "xmonad --recompile && xmonad --restart")

    -- Screenshots
  , ("<Print>",    spawn "scrot ~/Pictures/screenshots/%Y-%m-%d-%H%M%S.png")

    -- Volume keys
  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
  , ("<XF86AudioMute>",        spawn "pamixer -t")

    -- Brightness keys
  , ("<XF86MonBrightnessUp>",   spawn "brightnessctl set +5%")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
  ]
