import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens ( viewScreen
                                      , sendToScreen
                                      )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens ( withScreens
                                        , countScreens
                                        , marshallPP
                                        , onCurrentScreen
                                        , workspaces'
                                        )
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, hPutStrLn)

import System.Exit ( exitWith
                   , ExitCode (..)
                   )

import qualified XMonad.StackSet as W
import qualified Data.Map as Map


main = do
    screenNumber <- countScreens
    hs <- mapM (spawnPipe . xmobarCommand) [0..screenNumber - 1]
    xmonad $ defaults
        { workspaces = withScreens screenNumber myWorkspaces
        , logHook = mapM_ dynamicLogWithPP $ zipWith myBarPrettyPrinter hs [0..screenNumber]
        }

xmobarCommand (S screenNumber) = unwords [myStatusBar, "-x", show screenNumber]
myBarPrettyPrinter handle screenNumber = marshallPP screenNumber def 
    { ppVisible           = color "white"
    , ppUrgent            = color "red"
    , ppOrder             = \(wss:layout:title:_) -> [wss, layout, title]
    , ppOutput            = hPutStrLn handle
    , ppTitle = xmobarColor xmobarTitleColor "" . shorten 40 
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" 
    , ppSep = "  "
    , ppLayout = myLayoutPrinter
    }
    where color colorName = xmobarColor colorName ""

defaults = def
    { borderWidth = myBorderWidth
    , layoutHook = smartBorders $ avoidStruts $ myLayoutHook
    , focusFollowsMouse = myFocusFollowsMouse
    , focusedBorderColor = myFocusedBorderColor
    , manageHook = manageDocks <+> myManageHooks  <+> manageHook def
    -- To make Java applications behave normally...
    , startupHook = setWMName "LG3D" <+> setDefaultCursor xC_left_ptr <+> docksStartupHook
    , modMask = mod4Mask
    , mouseBindings = myMouseBindings
    , normalBorderColor  = myNormalBorderColor
    , terminal = myTerminal
    , keys = myKeys
    , handleEventHook = docksEventHook
    }

myLayoutPrinter :: String -> String
myLayoutPrinter "Full" = "<fn=1>\xf0b2</fn>"
myLayoutPrinter "Tall" = "<fn=1>\xf07e</fn>"
myLayoutPrinter "Mirror Tall" = "<fn=1>\xf07d</fn>"
myLayoutPrinter "Grid" = "<fn=1>\xf00a</fn>"
myLayoutPrinter x = x

myLayoutHook = layoutHook def ||| Grid
myModMask = mod4Mask
myBorderWidth = 1
myStatusBar = "xmobar"
myTerminal = "urxvtc"
myLauncher = "dmenu_run"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myNormalBorderColor  = "#928374"
myFocusedBorderColor = "#8ec07c"

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
xmobarTitleColor = "#d79921"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#b8bb26"

myKeys conf = let m = modMask conf in Map.fromList $
    [ ((myModMask, xK_p), spawn myLauncher)
    , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
    , ((myModMask .|. shiftMask, xK_c), kill)
    , ((myModMask, xK_q), restart "xmonad" True)
    , ((myModMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    , ((myModMask, xK_space), sendMessage NextLayout)
    , ((myModMask, xK_j), windows W.focusDown)
    , ((myModMask, xK_k), windows W.focusUp)
    , ((myModMask, xK_comma), sendMessage (IncMasterN 1))
    , ((myModMask, xK_period), sendMessage (IncMasterN (-1)))
    , ((myModMask, xK_m), windows W.focusMaster)
    , ((myModMask, xK_h), sendMessage Shrink)
    , ((myModMask, xK_l), sendMessage Expand)
    , ((myModMask, xK_t), withFocused $ windows . W.sink)
    , ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_Left), prevScreen)
    , ((myModMask .|. shiftMask, xK_Left), shiftPrevScreen)
    , ((myModMask, xK_Right),  nextScreen)
    , ((myModMask .|. shiftMask, xK_Right), shiftNextScreen)
    ] ++
    [ ((m .|. e .|. i, key), windows (onCurrentScreen f workspace)) 
      | (key, workspace) <- zip [xK_1..xK_9] (workspaces' conf)
      , (e, f)           <- [(0, W.view), (shiftMask, viewShift)]
    , i                  <- [0, controlMask, myModMask, controlMask .|. myModMask]
    ] ++
    [ ((myModMask .|. mask, key), f sc)
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]
    ]
        where viewShift i = W.view i . W.shift i
              withScreen screen f = screenWorkspace screen >>= flip whenJust (windows . f)

myMouseBindings (XConfig {XMonad.modMask = modMask}) = Map.fromList $
    [ -- mod-button1, Set the window to floating mode and move by dragging
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myWorkspaces = ["1 <fn=1>\xf269</fn>", "2 <fn=1>\xf120</fn>", "3 <fn=1>\xf02d</fn>", "4 <fn=1>\xf121</fn>", "5 media"] ++ map show [6..9]
myManageHooks = composeAll
    [ className =? "URxvt" --> doShift (myWorkspaces !! 1)
    , className =? "Firefox" --> doShift (myWorkspaces !! 0)
    , className =? "Zathura" --> doShift (myWorkspaces !! 2)
    , className =? "jetbrains-idea" --> doShift (myWorkspaces !! 3)
    , className =? "Easytag" --> doShift (myWorkspaces !! 4)
    , className =? "MPlayer" --> doShift (myWorkspaces !! 4)
    , className =? "feh" --> doShift (myWorkspaces !! 4)
    , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
    ]
