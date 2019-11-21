module Hooks.ManageHooks
    ( myManageHooks
    , myWorkspaces
    , manageSpotify
    ) where

import XMonad ( ManageHook
              , ScreenId
              , WorkspaceId
              )
import XMonad.Hooks.DynamicProperty
    ( dynamicPropertyChange )
import XMonad.Layout.IndependentScreens ( marshall
                                        , VirtualWorkspace
                                        )
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
                         , title
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
    [ className =? "Firefox" --> moveToWorkspace' [0] 0
    , className =? "Firefox" <&&> isDialog --> doCenterFloat
    , className =? "Chromium-browser" --> moveToWorkspace' [0] 0
    , className =? "Chromium-browser" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "Chromium" --> moveToWorkspace' [0] 0
    , className =? "Chromium" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "Spotify" --> moveToWorkspace' [1] 4
    , className =? "Spotify" <&&> isDialog --> doCenterFloat
    , className =? "Zathura" --> moveToWorkspace' [1] 3
    , className =? "feh" --> moveToWorkspace' [1] 3
    , className =? "octave-gui" --> moveToWorkspace' [1] 2
    , className =? "jetbrains-idea" --> moveToWorkspace' [1] 2
    , className =? "jetbrains-idea" <&&> resource =? "Dialog" --> moveToWorkspace' [1] 2
    , className =? "jetbrains-idea-ce" --> moveToWorkspace' [1] 2
    , className =? "jetbrains-idea-ce" <&&> resource =? "Dialog" --> moveToWorkspace' [1] 2
    , className =? "jetbrains-pycharm" --> moveToWorkspace' [1] 2
    , className =? "jetbrains-pycharm" <&&> resource =? "Dialog" --> moveToWorkspace' [1] 2
    , composeOne [ className =? "JFLAP" -?> moveToWorkspace' [1] 2
                 , transience
                 , className =? "JFLAP" <&&> isDialog -?> moveToWorkspace' [1] 2
                 ]
    , className =? "Easytag" --> moveToWorkspace' [1] 4
    , className =? "MPlayer" --> moveToWorkspace' [1] 4
    , className =? "mpv" --> moveToWorkspace' [1] 4
    , className =? "krita" --> moveToWorkspace' [1] 5
    , className =? "Wireshark" --> moveToWorkspace' [1] 0
    , className =? "Asunder" --> moveToWorkspace' [0] 4
    , className =? "Godot" --> moveToWorkspace' [1] 3
    , className =? "VirtualBox" --> moveToWorkspace' [1] 8
    , className =? "VirtualBox Manager" --> moveToWorkspace' [1] 8
    , className =? "VirtualBox" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "VirtualBox Manager" <&&> resource =? "Dialog" --> doCenterFloat
    , className =? "Postman" --> moveToWorkspace' [1] 0
    , className =? "Dunst" --> doFloat
    , name =? "mutt" --> moveToWorkspace' [1] 7
    , name =? "cmus" --> moveToWorkspace' [1] 4
    , name =? "ncmpcpp" --> moveToWorkspace' [1] 4
    , name =? "spt" --> moveToWorkspace' [1] 4
    , name =? "weechat" --> moveToWorkspace' [1] 6
    , name =? "newsboat" --> moveToWorkspace' [0] 3
    , name =? "ranger" --> moveToWorkspace' [1] 8
    , className =? "mutt" --> moveToWorkspace' [1] 7
    , className =? "cmus" --> moveToWorkspace' [1] 4
    , className =? "ncmpcpp" --> moveToWorkspace' [1] 4
    , className =? "weechat" --> moveToWorkspace' [1] 6
    , className =? "newsboat" --> moveToWorkspace' [0] 3
    , className =? "ranger" --> moveToWorkspace' [1] 8
    , className =? "URxvt" --> moveToWorkspace' [1] 1
    , className =? "Alacritty" --> moveToWorkspace' [1] 1
    , className =? "kitty" --> moveToWorkspace' [1] 1
    ]
        where
            moveToWorkspace' = moveToWorkspace screenNumber
            name = stringProperty "WM_NAME"

moveToWorkspace :: ScreenId -> [ScreenId] -> Int -> ManageHook
moveToWorkspace _ [] workspaceNumber = doShift $ marshall 0 (myWorkspaces !! workspaceNumber)
moveToWorkspace screenNumber (screenId:screenIds) workspaceNumber
    | screenId < screenNumber = doShift $ marshall screenId (chooseWorkspace workspaceNumber)
    | otherwise = moveToWorkspace screenNumber screenIds workspaceNumber
  where
      chooseWorkspace :: Int -> VirtualWorkspace
      chooseWorkspace = (!!) myWorkspaces

manageSpotify screenNumber =
    dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> moveToWorkspace screenNumber [1] 4)
