import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet  as W
import qualified Data.Map         as M
import qualified Data.List        as L

myWorkspaces = ["1:code", "2:webmail", "3:serialchat", "4:fun"] ++ map show [5..9]

-- I want these particular applications on particular workspaces
myManageHook = composeAll [ className =? "Amarok"     --> doF (W.shift "3:fun")
                          , className =? "Pidgin"     --> doF (W.shift "3:fun")
                          , className =? "Firefox"    --> doF (W.shift "2:webmail")
                          , className =? "Evolution"  --> doF (W.shift "2:webmail")
                          ]

main = dzen $ \conf -> xmonad $ conf
   { terminal    = "uxterm"
    , focusedBorderColor = "blue"
    , workspaces = myWorkspaces
    -- Consider my workspace preferences above plus my desire for dzen
    , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
    -- Avoid covering up dzen and other statusbars.
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    -- We will be using dzen2
    --, logHook = dynamicLogDzen
   }
