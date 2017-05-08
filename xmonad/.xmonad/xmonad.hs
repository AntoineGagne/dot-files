import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe, hPutStrLn)


main = do
    xmobarStatusBar <- xmobar $ def { terminal = terminalName
                                   , modMask = mod4Mask
                                   , borderWidth = 1
                                   }
    xmonad xmobarStatusBar

statusBarName = "xmobar"
terminalName = "urxvtc"
