import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens ( withScreens
                                        , countScreens
                                        , onCurrentScreen
                                        , workspaces'
                                        )
import XMonad.Layout.NoBorders
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
    barHandle <- statusBar "xmobar" ( 
        xmobarPP { ppTitle = xmobarColor xmobarTitleColor "" . shorten 40 
                 , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" 
                 , ppSep = "  "
                 , ppLayout = myLayoutPrinter
                 }
                                    )
            toggleStrutsKey $ defaults
                { workspaces = withScreens screenNumber myWorkspaces
                }
    xmonad barHandle

-- | The unexported X.H.DynamicLog.toggleStrutsKey
toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig { modMask = modm } = (modm, xK_b)

defaults = def
    { borderWidth = myBorderWidth
    , layoutHook = avoidStruts $ layoutHook def
    , focusFollowsMouse = myFocusFollowsMouse
    , focusedBorderColor = myFocusedBorderColor
    , manageHook = myManageHooks  <+> manageHook def <+> manageDocks
    -- To make Java applications behave normally...
    , startupHook = setWMName "LG3D" <+> setDefaultCursor xC_left_ptr
    , modMask = mod4Mask
    , mouseBindings = myMouseBindings
    , normalBorderColor  = myNormalBorderColor
    , terminal = myTerminal
    , keys = myKeys
    }

myLayoutPrinter :: String -> String
myLayoutPrinter "Full" = "F"
myLayoutPrinter "Tall" = "T"
myLayoutPrinter "Mirror Tall" = "M"
myLayoutPrinter x = x

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
    , ((myModMask, xK_w), withScreen 0 W.view)
    , ((myModMask .|. shiftMask  , xK_w), withScreen 0 viewShift)
    , ((myModMask, xK_e), withScreen 1 W.view)
    , ((myModMask .|. shiftMask, xK_e), withScreen 1 viewShift)
    ] ++
    [ ((m .|. e .|. i, key), windows (onCurrentScreen f workspace)) 
      | (key, workspace) <- zip [xK_1..xK_9] (workspaces' conf)
      , (e, f)           <- [(0, W.view), (shiftMask, viewShift)]
    , i                  <- [0, controlMask, myModMask, controlMask .|. myModMask]
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

myWorkspaces = ["1:web", "2:term", "3:read", "4:code", "5:media"] ++ map show [6..9]
myManageHooks = composeAll
    [ className =? "URxvt" --> doShift "2:term"
    , className =? "Firefox" --> doShift "1:web"
    , className =? "Zathura" --> doShift "3:read"
    , className =? "jetbrains-idea" --> doShift "4:code"
    , className =? "Easytag" --> doShift "5:media"
    , className =? "MPlayer" --> doShift "5:media"
    , className =? "feh" --> doShift "5:media"
    , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
    ]
