import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

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
  }
