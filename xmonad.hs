import XMonad
import XMonad.Config.Kde
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

import Data.Ratio ((%))

myWorkspaces = ["code", "web", "chat", "music", "mail", "movie", "bochs"]

myLayouts = smartBorders $ onWorkspace "chat" (gridIM (1%7) (Role "MainWindow#1")) $ onWorkspace "movie" Full $ layoutHook kde4Config

-- I want these particular applications on particular workspaces
myManageHook = composeAll [ className =? "Quodlibet" --> doShift "music"
                          , className =? "Pidgin" --> doShift "chat"
													, className =? "Kopete" --> doShift "chat"
                          , className =? "Shiretoko" --> doShift "web"
                          , className =? "IceWeasel" --> doShift "web"
                          , className =? "Evolution" --> doShift "mail"
													, className =? "Thunderbird-bin" --> doShift "mail"
                          , className =? "MPlayer" --> doShift "movie"
                          , className =? "xbmc.bin" --> doShift "movie"
                          , className =? "Bochs" --> doShift "bochs"
                          ]

main = xmonad $ kde4Config
		{ workspaces = myWorkspaces
    , modMask = mod4Mask -- Rebind Mod to the Windows key
		, manageHook = manageHook kde4Config <+> myManageHook
		, startupHook = startupHook kde4Config >> setWMName "LG3D"
    }
