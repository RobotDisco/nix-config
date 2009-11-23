import XMonad
import XMonad.Config.Kde
-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders

import Data.Ratio ((%))

myWorkspaces = ["code", "web", "chat", "music", "mail", "movie", "float"]

myLayouts = smartBorders $ onWorkspace "chat" (gridIM (1%7) (Role "buddy_list")) $ onWorkspace "movie" Full $ layoutHook kde4Config

-- I want these particular applications on particular workspaces
myManageHook = composeAll [ className =? "Quodlibet" --> doShift "music"
                          , className =? "Pidgin" --> doShift "chat"
													, className =? "Kopete" --> doShift "chat"
                          , className =? "Shiretoko" --> doShift "web"
                          , className =? "IceWeasel" --> doShift "web"
                          , className =? "Evolution" --> doShift "mail"
													, className =? "Thunderbird" --> doShift "mail"
                          , className =? "MPlayer" --> doShift "movie"
                          , className =? "xbmc.bin" --> doShift "movie"
                          , className =? "Bochs" --> doIgnore
                          ]

main = xmonad $ kde4Config
		{ workspaces = myWorkspaces
    , modMask = mod4Mask -- Rebind Mod to the Windows key
		, manageHook = manageHook kde4Config <+> myManageHook
    }
