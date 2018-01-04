-- vim: foldmethod=marker

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive ( fadeInactiveCurrentWSLogHook )
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens ( withScreens
                                        , countScreens
                                        , marshallPP
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
import XMonad.Util.Cursor ( setDefaultCursor )
import XMonad.Util.Run ( spawnPipe
                       , hPutStrLn
                       )

import XMonad.Programs.Terminals ( TerminalEmulator ( terminalDaemonName )
                                 , myTerminal
                                 )
import XMonad.Hooks.ManageHooks ( myManageHooks
                                , myWorkspaces
                                )
import XMonad.Hooks.Notifications ( myUrgencyHook )
import XMonad.Bindings.Keybindings ( myKeys
                                   , myLauncher
                                   , myModMask
                                   )
import XMonad.Bindings.MouseBindings ( myMouseBindings )
import XMonad.Themes.Fonts ( urxvtResourceFontString )
import XMonad.Themes.Gruvbox ( myTheme )
import XMonad.Themes.Palettes ( Palette (..) )
import XMonad.Themes.Themes ( Theme (..)
                            , showColor
                            )

main = do
    screenNumber <- countScreens
    hs <- mapM (spawnPipe . xmobarCommand) [0..screenNumber - 1]
    xmonad $ ewmh $ myUrgencyHook defaults
        { workspaces = withScreens screenNumber myWorkspaces
        , manageHook = manageDocks <+> myManageHooks screenNumber <+> manageHook def
        , logHook = fadeInactiveCurrentWSLogHook 0.8 <+> mapM_ dynamicLogWithPP (zipWith myBarPrettyPrinter hs [0..screenNumber])
        }

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
    { ppVisible = showMyThemeColor foreground
    , ppUrgent = showMyThemeColor color9
    , ppOrder = \(wss:layout:title:_) -> [wss, layout]
    , ppOutput = hPutStrLn handle
    , ppTitle = xmobarColor barTitleColor "" . shorten 40
    , ppCurrent = xmobarColor barCurrentWorkspaceColor ""
    , ppSep = "  "
    , ppLayout = myLayoutPrinter
    }
    where color colorName = xmobarColor colorName ""
          showMyThemeColor color' = color . showColor color' $ myTheme

-- Color of current window title in xmobar.
barTitleColor :: String
barTitleColor = showColor foreground myTheme

-- Color of current workspace in xmobar.
barCurrentWorkspaceColor :: String
barCurrentWorkspaceColor = showColor color10 myTheme


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
    , terminal = terminalDaemonName myTerminal
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
myNormalBorderColor  = showColor color8 myTheme

myFocusedBorderColor :: String
myFocusedBorderColor = showColor color14 myTheme
