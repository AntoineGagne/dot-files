module Hooks.ManageHooks
    ( myManageHooks
    , myWorkspaces
    ) where

import XMonad ( ManageHook
              , ScreenId
              , WorkspaceId
              )
import XMonad.Layout.IndependentScreens ( marshall )
import XMonad.Hooks.ManageHelpers ( composeOne
                                  , doCenterFloat
                                  , isDialog
                                  , transience
                                  , (-?>)
                                  )
import XMonad.ManageHook ( composeAll
                         , (-->)
                         , (=?)
                         , (<&&>)
                         , resource
                         , className
                         , stringProperty
                         , doFloat
                         , doShift
                         )

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1 <fn=1>\xf269</fn>"
               , "2 <fn=1>\xf120</fn>"
               , "3 <fn=1>\xf121</fn>"
               , "4 <fn=1>\xf02d</fn>"
               , "5 <fn=1>\xf001</fn>"
               , "6 <fn=1>\xf1fc</fn>"
               , "7 <fn=1>\xf27a</fn>"
               , "8 <fn=1>\xf0e0</fn>"
               , "9 <fn=1>\xf07c</fn>"
               ]

myManageHooks :: ScreenId -> ManageHook
myManageHooks screenNumber = composeAll
    [ className =? "Firefox" --> moveToWorkspace [0] 0
    , className =? "Firefox" <&&> isDialog --> doCenterFloat
    , className =? "Chromium-browser" --> moveToWorkspace [0] 0
    , className =? "Chromium-browser" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "Chromium" --> moveToWorkspace [0] 0
    , className =? "Chromium" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "Zathura" --> moveToWorkspace [1] 3
    , className =? "feh" --> moveToWorkspace [1] 3
    , className =? "octave-gui" --> moveToWorkspace [1] 2
    , className =? "jetbrains-idea" --> moveToWorkspace [1] 2
    , className =? "jetbrains-idea" <&&> resource =? "Dialog" --> moveToWorkspace [1] 2
    , className =? "jetbrains-pycharm" --> moveToWorkspace [1] 2
    , className =? "jetbrains-pycharm" <&&> resource =? "Dialog" --> moveToWorkspace [1] 2
    , composeOne [ className =? "JFLAP" -?> moveToWorkspace [1] 2
                 , transience
                 , className =? "JFLAP" <&&> isDialog -?> moveToWorkspace [1] 2
                 ]
    , className =? "Easytag" --> moveToWorkspace [1] 4
    , className =? "MPlayer" --> moveToWorkspace [1] 4
    , className =? "mpv" --> moveToWorkspace [1] 4
    , className =? "krita" --> moveToWorkspace [1] 5
    , className =? "Wireshark" --> moveToWorkspace [1] 0
    , className =? "Asunder" --> moveToWorkspace [0] 4
    , className =? "Godot" --> moveToWorkspace [1] 3
    , className =? "VirtualBox" --> moveToWorkspace [1] 8
    , className =? "VirtualBox Manager" --> moveToWorkspace [1] 8
    , className =? "VirtualBox" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "VirtualBox Manager" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "Postman" --> moveToWorkspace [1] 0
    , className =? "Dunst" --> doFloat
    , name =? "mutt" --> moveToWorkspace [1] 7
    , name =? "cmus" --> moveToWorkspace [1] 4
    , name =? "ncmpcpp" --> moveToWorkspace [1] 4
    , name =? "weechat" --> moveToWorkspace [1] 6
    , name =? "newsboat" --> moveToWorkspace [0] 3
    , className =? "mutt" --> moveToWorkspace [1] 7
    , className =? "cmus" --> moveToWorkspace [1] 4
    , className =? "ncmpcpp" --> moveToWorkspace [1] 4
    , className =? "weechat" --> moveToWorkspace [1] 6
    , className =? "newsboat" --> moveToWorkspace [0] 3
    , className =? "URxvt" --> moveToWorkspace [1] 1
    , className =? "Alacritty" --> moveToWorkspace [1] 1
    , className =? "kitty" --> moveToWorkspace [1] 1
    ]
        where moveToWorkspace :: [ScreenId] -> Int -> ManageHook
              moveToWorkspace [] workspaceNumber = doShift $ marshall 0 (chooseWorkspace workspaceNumber)
              moveToWorkspace (screenId:screenIds) workspaceNumber
                 | screenId < screenNumber = doShift $ marshall screenId (chooseWorkspace workspaceNumber)
                 | otherwise = moveToWorkspace screenIds workspaceNumber
              chooseWorkspace = (!!) myWorkspaces
              name = stringProperty "WM_NAME"
