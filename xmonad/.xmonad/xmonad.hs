import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Run (spawnPipe, hPutStrLn)

import qualified XMonad.StackSet as W
import qualified Data.Map as Map


main = do
    barHandle <- xmobar $ defaults 
        { layoutHook = avoidStruts $ layoutHook def
        , manageHook = myManageHooks  <+> manageHook def <+> manageDocks
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
            , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
            , ppSep = "   "
            }
        -- To make Java applications behave normally...
        , startupHook = setWMName "LG3D"
        }
    xmonad barHandle

defaults = def
    { borderWidth = myBorderWidth
    , focusFollowsMouse = myFocusFollowsMouse
    , focusedBorderColor = myFocusedBorderColor
    , manageHook = myManageHooks
    , modMask = mod4Mask
    , mouseBindings = myMouseBindings
    , normalBorderColor  = myNormalBorderColor
    , terminal = myTerminal
    , workspaces = myWorkspaces
    }

myModMask = mod4Mask
myBorderWidth = 1
myStatusBar = "xmobar"
myTerminal = "urxvtc"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#ffb6b0"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}
-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

myMouseBindings (XConfig {XMonad.modMask = modMask}) = Map.fromList $
    [ -- mod-button1, Set the window to floating mode and move by dragging
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myWorkspaces = ["1:web", "2:term", "3:read", "4:code", "5:media"] ++ map show [6..9]
myManageHooks = composeAll
    [ className =? "URxvt" --> doShift "2:term"
    , className =? "Firefox" --> doShift "1:web"
    , className =? "Zathura" --> doShift "3:read"
    , className =? "MPlayer" --> doShift "5:media"
    ]

