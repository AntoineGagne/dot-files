import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens ( viewScreen
                                      , sendToScreen
                                      )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive ( fadeInactiveCurrentWSLogHook )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens ( withScreens
                                        , countScreens
                                        , marshallPP
                                        , marshall
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
import XMonad.Hooks.WallpaperSetter ( defWallpaperConf
                                    , defWPNames
                                    , wallpaperSetter
                                    , WallpaperConf (..)
                                    , Wallpaper ( WallpaperDir )
                                    , WallpaperList (..)
                                    )
import qualified XMonad.Prompt         as Prompt
import qualified XMonad.Actions.Submap as Submap
import qualified XMonad.Actions.Search as Search
import Graphics.X11.Types ( xK_Print )
import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioLowerVolume
                                    , xF86XK_AudioMute
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioPlay
                                    , xF86XK_AudioStop
                                    , xF86XK_AudioPrev
                                    , xF86XK_AudioNext
                                    , xF86XK_MonBrightnessUp
                                    , xF86XK_MonBrightnessDown
                                    )

import System.Exit ( exitWith
                   , ExitCode (..)
                   )

import qualified XMonad.StackSet as W
import qualified Data.Map as Map


main = do
    screenNumber <- countScreens
    hs <- mapM (spawnPipe . xmobarCommand) [0..screenNumber - 1]
    spawn "display-screens"
    xmonad $ defaults
        { workspaces = withScreens screenNumber myWorkspaces
        , manageHook = manageDocks <+> myManageHooks screenNumber <+> manageHook def
        , logHook = fadeInactiveCurrentWSLogHook 0.8 <+> (mapM_ dynamicLogWithPP $ zipWith myBarPrettyPrinter hs [0..screenNumber])
        }

-- TODO: Find a way to make it work with the multi-head setup
myWallpaperSetterHook :: X ()
myWallpaperSetterHook = wallpaperSetter defWallpaperConf 
    { wallpapers = WallpaperList $ zip (workspaces' defaults) $ replicate (length myWorkspaces) (WallpaperDir "~/.wallpapers")
    }

xmobarCommand :: ScreenId -> String
xmobarCommand (S screenNumber) = unwords [ myStatusBar
                                         , "-x"
                                         , show screenNumber
                                         , additionalCommands
                                         , "-i"
                                         , "~"
                                         ]
    where additionalCommands = "-C '[Run PipeReader \"N/A:/$HOME/.volume-" ++ show screenNumber ++ "\" \"vol\", Run PipeReader \"Nothing Playing:$HOME/.song-information-" ++ show screenNumber ++ "\" \"song\"]'"


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
myLayoutPrinter "Full" = "<icon=.icons/layout/full_screen_layout.xpm/>"
myLayoutPrinter "Tall" = "<icon=.icons/layout/vertical_layout.xpm/>"
myLayoutPrinter "Mirror Tall" = "<icon=.icons/layout/horizontal_layout.xpm/>"
myLayoutPrinter "Grid" = "<icon=.icons/layout/grid_layout.xpm/>"
myLayoutPrinter x = x

myLayoutHook = layoutHook def ||| Grid

myModMask = mod4Mask

myBorderWidth :: Dimension
myBorderWidth = 1

myStatusBar :: String
myStatusBar = "xmobar"

myTerminal :: String
myTerminal = "urxvtc"

myLauncher :: String
myLauncher = "dmenu_run"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myNormalBorderColor :: String
myNormalBorderColor  = "#928374"

myFocusedBorderColor :: String
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
xmobarTitleColor :: String
xmobarTitleColor = "#d79921"
-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor :: String
xmobarCurrentWorkspaceColor = "#b8bb26"

-- {{{1 Keybindings
myKeys :: (XConfig Layout -> Map.Map (ButtonMask, KeySym) (X ()))
myKeys conf = let m = modMask conf in Map.fromList $
    -- {{{2 Programs
    [ ((myModMask, xK_p), spawn myLauncher)
    , ((myModMask .|. shiftMask, xK_Return), spawn myTerminal)
    , ((myModMask .|. shiftMask, xK_c), kill)
    -- {{{2 XMonad
    , ((myModMask, xK_q), restart "xmonad" True)
    , ((myModMask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
    -- {{{2 Layouts
    , ((myModMask, xK_space), sendMessage NextLayout)
    -- Reset the layouts on the current workspace
    , ((myModMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
    -- {{{2 Focus
    -- Move focus to the next window
    , ((myModMask, xK_Tab), windows W.focusDown)
    , ((myModMask, xK_j), windows W.focusDown)
    , ((myModMask, xK_k), windows W.focusUp)
    , ((myModMask, xK_m), windows W.focusMaster)
    -- Swap the focused window with the next window
    , ((myModMask .|. shiftMask, xK_j), windows W.swapDown)
    -- Swap the focused window with the previous window
    , ((myModMask .|. shiftMask, xK_k), windows W.swapUp)
    -- Swap the focused window and the master window
    , ((myModMask, xK_Return), windows W.swapMaster)
    -- {{{2 Sizing
    -- Increment the number of windows in the master area
    , ((myModMask, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((myModMask, xK_period), sendMessage (IncMasterN (-1)))
    -- Resize viewed windows to the correct size
    , ((myModMask, xK_n), refresh)
    -- Shrink the master area
    , ((myModMask, xK_h), sendMessage Shrink)
    -- Expand the master area
    , ((myModMask, xK_l), sendMessage Expand)
    -- Push window back into tiling
    , ((myModMask, xK_t), withFocused $ windows . W.sink)
    , ((myModMask, xK_b), sendMessage ToggleStruts)
    -- {{{2 Screenshots
    , ((0, xK_Print), spawn "printscreen -f")
    , ((myModMask, xK_Print), spawn "printscreen -s")
    -- {{{2 Audio Controls
    , ((0, xF86XK_AudioMute), spawn "control-volume -t")
    , ((0, xF86XK_AudioLowerVolume), spawn "control-volume -c -5%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "control-volume -c +5%")
    -- {{{2 Music Controls
    , ((0, xF86XK_AudioNext), spawn "cmus-remote -n")
    , ((0, xF86XK_AudioPrev), spawn "cmus-remote -r")
    , ((0, xF86XK_AudioStop), spawn "cmus-remote -s")
    , ((0, xF86XK_AudioPlay), spawn "cmus-remote -u")
    -- {{{2 Brightness Controls
    , ((0, xF86XK_MonBrightnessDown), spawn "control-brightness -5")
    , ((0, xF86XK_MonBrightnessUp), spawn "control-brightness 5")
    -- {{{2 Screens Related
    , ((myModMask, xK_Left), prevScreen)
    , ((myModMask .|. shiftMask, xK_Left), shiftPrevScreen)
    , ((myModMask, xK_Right),  nextScreen)
    , ((myModMask .|. shiftMask, xK_Right), shiftNextScreen)
    -- {{{2 Search Engines
    , ((myModMask, xK_s), Submap.submap $ searchEngineMap $ Search.promptSearch myPrompt)
    , ((myModMask .|. shiftMask, xK_s), Submap.submap $ searchEngineMap $ Search.selectSearch)
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
              searchEngineMap method = Map.fromList $
                  [ ((0, xK_a), method Search.amazon)
                  , ((0, xK_h), method Search.hoogle)
                  , ((0, xK_i), method Search.images)
                  , ((0, xK_m), method Search.imdb)
                  , ((0, xK_s), method Search.stackage)
                  , ((0, xK_w), method Search.wikipedia)
                  , ((0, xK_y), method Search.youtube)
                  , ((0, xK_g), method Search.google)
                  ]

myPrompt = Prompt.def { Prompt.font = "xft:xft:Source Code Pro:style=Bold:size=9:antialias=true"
                      , Prompt.bgColor = "#282828"
                      , Prompt.fgColor = "#d5c4a1"
                      , Prompt.fgHLight = "#b8bb26"
                      , Prompt.bgHLight = "#282828"
                      , Prompt.promptBorderWidth = 1
                      , Prompt.borderColor = "#3c3836"
                      }

myMouseBindings :: (XConfig Layout -> Map.Map (ButtonMask, Button) (Window -> X ()))
myMouseBindings (XConfig {XMonad.modMask = modMask}) = Map.fromList $
    [ -- mod-button1, Set the window to floating mode and move by dragging
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1 <fn=1>\xf269</fn>", "2 <fn=1>\xf120</fn>", "3 <fn=1>\xf02d</fn>", "4 <fn=1>\xf121</fn>", "5 <fn=1>\xf11b</fn>", "6 <fn=1>\xf1fc</fn>"] ++ map show [7..9]

myManageHooks :: ScreenId -> ManageHook
myManageHooks screenNumber = composeAll $
    [ className =? "URxvt" --> moveToWorkspace [1] 1
    , className =? "Firefox" --> moveToWorkspace [0] 0
    , className =? "Chromium-browser" --> moveToWorkspace [0] 0
    , className =? "Zathura" --> moveToWorkspace [1] 2
    , className =? "feh" --> moveToWorkspace [1] 2
    , className =? "jetbrains-idea" --> moveToWorkspace [1] 3
    , className =? "Easytag" --> moveToWorkspace [1] 4
    , className =? "MPlayer" --> moveToWorkspace [1] 4
    , className =? "mpv" --> moveToWorkspace [1] 4
    , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
    , className =? "krita" --> moveToWorkspace [1] 5
    , className =? "Wireshark" --> moveToWorkspace [1] 0
    ]
        where moveToWorkspace :: [ScreenId] -> Int -> ManageHook
              moveToWorkspace [] workspaceNumber = doShift $ marshall 0 (chooseWorkspace workspaceNumber)
              moveToWorkspace (screenId:screenIds) workspaceNumber
                 | screenId < screenNumber = doShift $ marshall screenId (chooseWorkspace workspaceNumber)
                 | otherwise = moveToWorkspace screenIds workspaceNumber
              chooseWorkspace = (!!) myWorkspaces
