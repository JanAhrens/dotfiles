import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.Volume
import XMonad.Util.Run(spawnPipe)

import System.IO(hPutStrLn)

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig {
      terminal = "konsole"

    , borderWidth        = 3
    , normalBorderColor  = "#1E1C10"
    , focusedBorderColor = "#5D0017"

    , manageHook =
            manageDocks

        -- fix full screen layout for videos etc.
        <+> composeAll [isFullscreen --> doFullFloat]

        -- use default manageHook
        <+> manageHook defaultConfig

    , layoutHook =

        -- automatically remove borders from full screen layouts
          smartBorders

        -- use default layoutHook
        $ avoidStruts (layoutHook defaultConfig) ||| Full

    -- use the ,,windows key'' as mod key
    , modMask    = mod4Mask
    , logHook    = dynamicLogWithPP $ xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "#cdcd57" "" . shorten 50
                    , ppCurrent = xmobarColor "#cdcd57" ""
                    , ppSep = " <fc=#3d3d07>|</fc> "
                    }
  } `additionalKeysP` [ ("<XF86AudioMute>",         toggleMute    >> return())
                      , ("<XF86AudioLowerVolume>",  lowerVolume 4 >> return())
                      , ("<XF86AudioRaiseVolume>",  raiseVolume 4 >> return())
                      ]
