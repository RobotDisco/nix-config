import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import qualified Data.Map         as M
import qualified Data.List        as L
import Data.Ratio ((%))

myWorkspaces = ["1:code", "2:web", "3:IM", "4:fun", "5:mail", "6:sgos"] ++ map show [7..9]

myLayouts = onWorkspace "3:IM" (gridIM (1%7) (Role "buddy_list")) $ layoutHook defaultConfig

-- I want these particular applications on particular workspaces
myManageHook = composeAll [ className =? "quodlibet"  --> doShift "4:fun"
                          , className =? "Pidgin"     --> doShift "3:IM"
                          , className =? "Gran Paradiso" --> doShift "2:web"
                          , className =? "Iceweasel" --> doShift "2:web"
													, className =? "Evolution" --> doShift "5:mail"
                          ]

main = xmobar $ \conf -> xmonad $ conf
   { terminal    = "urxvt"
    , focusedBorderColor = "blue"
    , workspaces = myWorkspaces
    -- Consider my workspace preferences above plus my desire for dzen
    , manageHook = myManageHook <+> manageDocks
    -- Avoid covering up dzen and other statusbars.
    , layoutHook = avoidStruts $ myLayouts
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    -- We will be using dzen2
    --, logHook = dynamicLogDzen
   }
