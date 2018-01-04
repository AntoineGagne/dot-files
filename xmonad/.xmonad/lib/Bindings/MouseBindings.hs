module Bindings.MouseBindings
    ( myMouseBindings
    ) where

import XMonad ( XConfig (..)
              , Layout
              , ButtonMask
              , Button
              , Window
              , X
              , windows
              , focus
              , mouseMoveWindow
              , button1
              , button2
              , button3
              )
import XMonad.Actions.FlexibleResize ( mouseResizeEdgeWindow )

import qualified XMonad as XMonad
import qualified XMonad.StackSet as StackSet
import qualified Data.Map as Map

myMouseBindings :: (XConfig Layout -> Map.Map (ButtonMask, Button) (Window -> X ()))
myMouseBindings XConfig {XMonad.modMask = modMask} = Map.fromList
    [ -- mod-button1, Set the window to floating mode and move by dragging
      ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows StackSet.swapMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeEdgeWindow 10 w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
