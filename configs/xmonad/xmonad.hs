import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, shorten, wrap, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import System.IO (hPutStrLn)
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import System.Exit (exitSuccess)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
  xmonad .
    ewmh .
    docks $
    def
      { modMask = mod4Mask
      , terminal = "kitty"
      , layoutHook = myLayout
      , manageHook = manageDocks <+> manageHook def
      , logHook = dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "#ffffff" "" . shorten 60
          , ppCurrent = xmobarColor "#7aa2f7" "" . wrap "[" "]"
          }
      }
    `additionalKeysP`
      [ ("M-<Return>", spawn "kitty")
      , ("M-d", spawn "dmenu_run")
      , ("M-b", spawn "firefox")
      , ("M-S-r", spawn "xmonad --recompile; xmonad --restart")
      , ("M-S-q", io exitSuccess)
      ]

myLayout =
  avoidStruts
    $ smartBorders
    $ spacingWithEdge 6
    $ Tall 1 (3 / 100) (1 / 2)
    ||| Full
