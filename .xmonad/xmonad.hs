import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)

import XMonad.Actions.Volume
import XMonad.Actions.CycleWS (nextWS,prevWS,toggleWS,shiftToNext,shiftToPrev)

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Named
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)

import System.IO(hPutStrLn)

myWorkspaces = ["1:main", "2:web", "3:talk", "4:virtual"]

myLayoutHook =  workspaceConf $ lessBorders OnlyFloat $ grid ||| tall ||| full
  where myNamed n = named n . spacing 6 . avoidStruts
        grid   = myNamed "grid" Grid
        tall   = myNamed "tall" $ Tall 1 (3/100) (1/2)
        full   = myNamed "full" Full
        workspaceConf = onWorkspace (myWorkspaces !! 0) (noBorders Full)

myKeys = [ ("M-<Left>",  prevWS)
         , ("M-<Right>", nextWS)
         , ("C-S-l", spawn "gnome-screensaver-command --lock")
         , ("<XF86AudioMute>",        toggleMute    >> return())
         , ("<XF86AudioLowerVolume>", lowerVolume 4 >> return())
         , ("<XF86AudioRaiseVolume>", raiseVolume 4 >> return())
         ]

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig {
      terminal = "urxvt"

    , borderWidth        = 3
    , normalBorderColor  = "#073642"
    , focusedBorderColor = "#dc322f"

    , workspaces = myWorkspaces

    , focusFollowsMouse = True

    , manageHook =
            manageDocks

        -- fix full screen layout for videos etc.
        <+> composeAll [isFullscreen --> doFullFloat]

        -- use default manageHook
        <+> manageHook defaultConfig

    , layoutHook = myLayoutHook

    -- use the ,,windows key'' as mod key
    , modMask    = mod4Mask
    , logHook    = dynamicLogWithPP $ xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "#cdcd57" "" . shorten 50
                    , ppCurrent = xmobarColor "#cdcd57" ""
                    , ppSep = " <fc=#3d3d07>|</fc> "
                    }
  } `additionalKeysP` myKeys
