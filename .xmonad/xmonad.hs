import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.Volume

main = do
  xmonad $ defaultConfig {
      manageHook =
            manageDocks

        -- fix full screen layout for videos etc.
        <+> composeAll [isFullscreen --> doFullFloat]

        -- use default manageHook
        <+> manageHook defaultConfig

    , layoutHook =
          avoidStruts

        -- automatically remove borders from full screen layouts
        $ smartBorders

        -- use default layoutHook
        $ layoutHook defaultConfig

    -- use the ,,windows key'' as mod key
    , modMask    = mod4Mask
  } `additionalKeysP` [ ("<XF86AudioMute>", toggleMute >> return()),
    ("<XF86AudioLowerVolume>",  lowerVolume 4 >> return()),
    ("<XF86AudioRaiseVolume>",  raiseVolume 4 >> return())]
