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

myWorkspaces = ["1:main", "2:web", "3:talk", "4:virtual", "5", "6", "7", "8", "9"]

myTheme = defaultTheme
        { inactiveColor = "#073642"
        , activeColor   = "#dc322f"
        , fontName      = "xft:Terminus-8"
        }

myLayoutHook =  workspaceConf $ lessBorders OnlyFloat $ grid ||| tall ||| full
  where myNamed n = named n . avoidStruts
        grid   = (myNamed "grid") . (spacing 6) $ Grid
        tall   = (myNamed "tall") . (spacing 6) $ Tall 1 (3/100) (1/2)
        full   = myNamed "full" . smartBorders $ Full
        workspaceConf = onWorkspace (myWorkspaces !! 0) ((avoidStruts . noBorders $ tabbedBottom shrinkText myTheme) ||| noBorders Full)

myKeys = [ ("M-<Left>",  prevWS)
         , ("M-<Right>", nextWS)
         , ("C-S-l", spawn "gnome-screensaver-command --lock")
         , ("<XF86AudioMute>",        toggleMute    >> return())
         , ("<XF86AudioLowerVolume>", lowerVolume 4 >> return())
         , ("<XF86AudioRaiseVolume>", raiseVolume 4 >> return())
         ]

myLogHook xmobar = dynamicLogWithPP $ xmobarPP
                {
                -- applied to the entire formatted string in order to output it
                  ppOutput = hPutStrLn xmobar
                -- how to print the tag of the currently focused workspace
                , ppCurrent = xmobarColor "#dc322f" ""
                --  window title format 
                , ppTitle = xmobarColor "#dc322f" ""
                -- layout name format
                , ppLayout = const ""
                , ppSep = " ❙ "
                , ppWsSep = " ⚫ "
                }

myManageHook d = manageDocks
              -- fix full screen layout for videos etc.
              <+> composeAll [isFullscreen --> doFullFloat]

              -- use default manageHook
              <+> manageHook d

main = do
  xmobarProc <- spawnPipe "~/.cabal/bin/xmobar"
  xmonad $ defaultConfig {
      terminal           = "urxvt"
    , borderWidth        = 3
    , normalBorderColor  = "#073642"
    , focusedBorderColor = "#dc322f"
    , focusFollowsMouse  = True
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook defaultConfig
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmobarProc
  } `additionalKeysP` myKeys
