-- vim: foldmethod=marker

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
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize ( mouseResizeEdgeWindow )
import XMonad.Actions.PhysicalScreens ( viewScreen
                                      , sendToScreen
                                      )
import XMonad.Actions.Search ( (!>) )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive ( fadeInactiveCurrentWSLogHook )
import XMonad.Hooks.EwmhDesktops ( ewmh 
                                 , ewmhDesktopsStartup
                                 )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Hooks.UrgencyHook ( withUrgencyHook
                                , withUrgencyHookC
                                , NoUrgencyHook (..)
                                , focusUrgent
                                , clearUrgents
                                , borderUrgencyHook
                                , BorderUrgencyHook (..)
                                , urgencyConfig
                                , UrgencyHook (..)
                                , RemindWhen (..)
                                , SuppressWhen (..)
                                , minutes
                                , UrgencyConfig (..)
                                )
import XMonad.Hooks.WallpaperSetter ( defWallpaperConf
                                    , defWPNames
                                    , wallpaperSetter
                                    , WallpaperConf (..)
                                    , Wallpaper ( WallpaperDir )
                                    , WallpaperList (..)
                                    )
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens ( withScreens
                                        , countScreens
                                        , marshallPP
                                        , marshall
                                        , onCurrentScreen
                                        , workspaces'
                                        )
import XMonad.Layout.Column ( Column (..) )
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.OneBig ( OneBig (..) )
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, spawnPipe, hPutStrLn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.Prompt         as Prompt
import qualified XMonad.Actions.Submap as Submap
import qualified XMonad.Actions.Search as Search

import qualified XMonad.StackSet as W
import qualified Data.Map as Map

import XMonad.Hooks.ManageHooks ( myManageHooks
                                , myWorkspaces
                                )
import XMonad.Hooks.Notifications ( LibNotifyUrgencyHook (..) )
import XMonad.Programs.Terminals ( urxvt
                                 , launchApp
                                 , mutt
                                 , ncmpcpp
                                 , newsboat
                                 , weechat
                                 , TerminalEmulator (..)
                                 )
import XMonad.Bindings.Keybindings ( myKeys
                                   , myLauncher
                                   , myTerminal
                                   , myModMask
                                   )
import XMonad.Themes.Fonts ( urxvtResourceFontString )
import XMonad.Themes.Gruvbox ( gruvboxTheme )
import XMonad.Themes.Palettes ( Palette (..) )
import XMonad.Themes.Themes ( Theme (..) )


main = do
    screenNumber <- countScreens
    hs <- mapM (spawnPipe . xmobarCommand) [0..screenNumber - 1]
    xmonad $ ewmh $ withUrgencyHookC LibNotifyUrgencyHook urgencyConfig { suppressWhen = Visible, remindWhen = Every (minutes 5.0 )} $ defaults
        { workspaces = withScreens screenNumber myWorkspaces
        , manageHook = manageDocks <+> myManageHooks screenNumber <+> manageHook def
        , logHook = fadeInactiveCurrentWSLogHook 0.8 <+> mapM_ dynamicLogWithPP (zipWith myBarPrettyPrinter hs [0..screenNumber])
        }

-- TODO: Find a way to make it work with the multi-head setup
myWallpaperSetterHook :: ScreenId -> X ()
myWallpaperSetterHook screenNumber = wallpaperSetter defWallpaperConf
    { wallpapers = WallpaperList $ zip marshalledWorkspaces $ replicate (length myWorkspaces) (WallpaperDir "~/.wallpapers/")
    , wallpaperBaseDir = "~/.wallpapers/"
    }
        where marshalledWorkspaces = [marshall id marshalledWorkspace | marshalledWorkspace <- myWorkspaces, id <- [0..screenNumber - 1]]

xmobarCommand :: ScreenId -> String
xmobarCommand (S screenNumber) = unwords [ myStatusBar
                                         , "-x"
                                         , show screenNumber
                                         , additionalCommands
                                         , "-i"
                                         , "~"
                                         ]
    where additionalCommands = "-C '[Run PipeReader \"N/A:/$HOME/.volume-" ++ show screenNumber ++ "\" \"vol\"]'"


myBarPrettyPrinter handle screenNumber = marshallPP screenNumber def
    { ppVisible = color . show . foreground . palette $ gruvboxTheme
    , ppUrgent = color . show . color9 . palette $ gruvboxTheme
    , ppOrder = \(wss:layout:title:_) -> [wss, layout]
    , ppOutput = hPutStrLn handle
    , ppTitle = xmobarColor barTitleColor "" . shorten 40
    , ppCurrent = xmobarColor barCurrentWorkspaceColor ""
    , ppSep = "  "
    , ppLayout = myLayoutPrinter
    }
    where color colorName = xmobarColor colorName ""

-- Color of current window title in xmobar.
barTitleColor :: String
barTitleColor = show . foreground . palette $ gruvboxTheme

-- Color of current workspace in xmobar.
barCurrentWorkspaceColor :: String
barCurrentWorkspaceColor = show . color10 . palette $ gruvboxTheme


defaults = def
    { borderWidth = myBorderWidth
    , layoutHook = smartBorders $ avoidStruts myLayoutHook
    , focusFollowsMouse = myFocusFollowsMouse
    , focusedBorderColor = myFocusedBorderColor
    -- To make Java applications behave normally...
    , startupHook = setWMName "LG3D" <+> setDefaultCursor xC_left_ptr <+> docksStartupHook
    , modMask = mod4Mask
    , mouseBindings = myMouseBindings
    , normalBorderColor  = myNormalBorderColor
    , terminal = show myTerminal
    , keys = myKeys
    , handleEventHook = docksEventHook
    }

myLayoutPrinter :: String -> String
myLayoutPrinter "Full" = "<icon=.icons/layout/alternate_full_screen_layout.xpm/>"
myLayoutPrinter "Tall" = "<icon=.icons/layout/vertical_layout.xpm/>"
myLayoutPrinter "Mirror Tall" = "<icon=.icons/layout/horizontal_layout.xpm/>"
myLayoutPrinter "Grid" = "<icon=.icons/layout/grid_layout.xpm/>"
myLayoutPrinter ('O':'n':'e':'B':'i':'g':_) = "<icon=.icons/layout/one_big_layout.xpm/>"
myLayoutPrinter ('C':'o':'l':'u':'m':'n':_) = "<icon=.icons/layout/column_layout.xpm/>"
myLayoutPrinter x = x

myLayoutHook = layoutHook def
            ||| Grid
            ||| OneBig (3/4) (3/4)
            ||| Column 1.6

myBorderWidth :: Dimension
myBorderWidth = 2

myStatusBar :: String
myStatusBar = "xmobar"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myNormalBorderColor :: String
myNormalBorderColor  = show . color8 . palette $ gruvboxTheme

myFocusedBorderColor :: String
myFocusedBorderColor = show . color14 . palette $ gruvboxTheme

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def
    { activeBorderColor = show . color7 . palette $ gruvboxTheme
    , activeTextColor = show . foreground . palette $ gruvboxTheme
    , activeColor = show . color6 . palette $ gruvboxTheme
    , inactiveBorderColor = show . color7 . palette $ gruvboxTheme
    , inactiveTextColor = show . foreground . palette $ gruvboxTheme
    , inactiveColor = show . background . palette $ gruvboxTheme
    }

myMouseBindings :: (XConfig Layout -> Map.Map (ButtonMask, Button) (Window -> X ()))
myMouseBindings XConfig {XMonad.modMask = modMask} = Map.fromList
    [ -- mod-button1, Set the window to floating mode and move by dragging
      ((modMask, button1), \w -> focus w >> mouseMoveWindow w)
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), \w -> focus w >> windows W.swapMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeEdgeWindow 10 w)
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
