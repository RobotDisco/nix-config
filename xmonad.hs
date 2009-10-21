import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Util.Run
import XMonad.Util.EZConfig

import Data.List (isPrefixOf)
import Data.Ratio ((%))

myWorkspaces = ["1:code", "2:web", "3:IM", "4:fun", "5:mail", "6:xbmc"] ++ map show [7..8] ++ ["9:float"]

myLayouts = onWorkspace "3:IM" (gridIM (1%7) (Role "buddy_list")) $ onWorkspace "6:xbmc" Full $ smartBorders $ layoutHook defaultConfig

-- I want these particular applications on particular workspaces
myManageHook = composeAll [ className =? "Quodlibet"  --> doShift "4:fun"
                          , className =? "Pidgin"     --> doShift "3:IM"
                          , className =? "Shiretoko" --> doShift "2:web"
                          , className =? "IceWeasel" --> doShift "2:web"
                          , className =? "Evolution" --> doShift "5:mail"
                          , className =? "MPlayer" --> doFloat
                          , className =? "xbmc.bin" --> doShift "6:xbmc"
                          , className =? "Bochs" --> doShift "9:float"
                          ]

main = do
  h <- spawnPipe "xmobar"     
  xmonad $ defaultConfig
    { terminal    = "urxvt"
    , focusedBorderColor = "blue"
    , workspaces = myWorkspaces
    -- Consider my workspace preferences above plus my desire for dzen
    , manageHook = myManageHook <+> manageDocks
    -- Avoid covering up dzen and other statusbars.
    , layoutHook = avoidStruts $ myLayouts
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    -- Pipe our statusbar info to Xmonad
    , logHook = dynamicLogWithPP $ xmobarPP
        { ppOutput = hPutStrLn h
        , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
        }
    -- Hack for Java programs to display properly
    , startupHook = setWMName "LG3D"
    } `additionalKeysP`
    [ ("M-\\", spawn "xkill")
    , ("M-f", spawn "firefox")
    , ("M-z", spawn "gvim")
    , ("M-v", spawn "pavucontrol")
    , ("M-n", spawn "wicd-client -n")
    , ("M-x", spawn "xscreensaver-command -lock")
    ]
  
-- | Strip xmobar markup. Useful to remove ppHidden color from ppUrgent
--   field. For example:
--
-- >     , ppHidden          = xmobarColor "gray20" "" . wrap "<" ">"
-- >     , ppUrgent          = xmobarColor "dark orange" "" .  xmobarStrip
xmobarStrip :: String -> String
xmobarStrip = strip [] where
    strip keep x
      | null x                 = keep
      | "<fc="  `isPrefixOf` x = strip keep (drop 1 . dropWhile (/= '>') $ x)
      | "</fc>" `isPrefixOf` x = strip keep (drop 5  x)
      | '<' == head x          = strip (keep ++ "<") (tail x)
      | otherwise              = let (good,x') = span (/= '<') x
                                 in strip (keep ++ good) x'

