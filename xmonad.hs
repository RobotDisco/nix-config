import XMonad
-- import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Util.Run
import XMonad.Util.EZConfig

import Data.List (isPrefixOf)
import Data.Ratio ((%))

myWorkspaces = ["work", "web", "chat", "music", "mail", "movie", "bochs"]

myLayouts = smartBorders $ onWorkspace "chat" (gridIM (1%7) (Role "buddy_list")) $ onWorkspace "movie" Full $ layoutHook defaultConfig

-- I want these particular applications on particular workspaces
myManageHook = composeAll [ className =? "Quodlibet" --> doShift "music"
													, className =? "Amarok" --> doShift "music"
													, className =? "Songbird" --> doShift "music"
                          , className =? "Pidgin" --> doShift "chat"
													, className =? "Kopete" --> doShift "chat"
                          , className =? "Namoroka" --> doShift "web"
                          , className =? "IceWeasel" --> doShift "web"
                          , className =? "Chromium" --> doShift "web"
                          , className =? "Evolution" --> doShift "mail"
													, className =? "Thunderbird-bin" --> doShift "mail"
                          , className =? "MPlayer" --> doShift "movie"
                          , className =? "xbmc.bin" --> doShift "movie"
                          , className =? "Bochs" --> doShift "bochs"
                          ]

main = do
  h <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , modMask = mod4Mask -- Rebind Mod to the Windows key
    , focusedBorderColor = "blue"
    , workspaces = myWorkspaces
    , layoutHook = avoidStruts myLayouts
    -- Consider my workspace preferences above plus my desize for xmonad
    , manageHook = manageHook defaultConfig <+> myManageHook <+> manageDocks
    -- Pipe out statusbar info into xmobar
    , logHook = dynamicLogWithPP $ xmobarPP
      { ppOutput = hPutStrLn h
      }
    -- Hack for Java programs to display properly
    , startupHook = startupHook defaultConfig >> setWMName "LG3D"
    } `additionalKeysP`
    [ ("M-\\", spawn "xkill")
    , ("M-F", spawn "firefox")
    , ("M-f", spawn "chromium")
    , ("M-s", spawn "/home/gaelan/slickedit/bin/vs")
    , ("M-z", spawn "gvim")
    , ("M-n", spawn "wicd-client -n")
    , ("M-x", spawn "xscreensaver-command -lock")
    ]
