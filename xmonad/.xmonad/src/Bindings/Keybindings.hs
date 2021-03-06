module Bindings.Keybindings
  ( myKeys,
    myLauncher,
    myModMask,
  )
where

import Data.Default
import qualified Data.Map as Map
import Data.Maybe
  ( fromJust,
  )
import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume,
    xF86XK_AudioMute,
    xF86XK_AudioNext,
    xF86XK_AudioPlay,
    xF86XK_AudioPrev,
    xF86XK_AudioRaiseVolume,
    xF86XK_AudioStop,
    xF86XK_MonBrightnessDown,
    xF86XK_MonBrightnessUp,
  )
import qualified Programs.Brightness as PBrightness
import Programs.Commands (runCommand)
import Programs.MusicPlayers
  ( MusicPlayerControls (..),
    myMusicPlayer,
  )
import Programs.Terminals
  ( muttCommand,
    myTerminalCommand,
    newsboatCommand,
    rangerCommand,
    sptCommand,
    weechatCommand,
  )
import Programs.Volume (myVolumeControl)
import qualified Programs.Volume as PVolume
import Prompts.SearchPrompts (myPrompt)
import System.Exit (exitSuccess)
import Themes.Gruvbox (myGridConfig)
import XMonad
import XMonad.Actions.CycleWS
  ( nextScreen,
    prevScreen,
    shiftNextScreen,
    shiftPrevScreen,
  )
import XMonad.Actions.GridSelect
  ( goToSelected,
  )
import XMonad.Actions.PhysicalScreens
  ( sendToScreen,
    viewScreen,
  )
import XMonad.Actions.Search ((!>))
import qualified XMonad.Actions.Search as Search
import qualified XMonad.Actions.Submap as Submap
import XMonad.Hooks.ManageDocks (ToggleStruts (..))
import XMonad.Hooks.UrgencyHook
  ( clearUrgents,
    focusUrgent,
  )
import XMonad.Layout.IndependentScreens
  ( onCurrentScreen,
    workspaces',
  )
import XMonad.Prompt.Window
  ( WindowPrompt (Goto),
    allWindows,
    windowPrompt,
  )
import qualified XMonad.StackSet as StackSet
import XMonad.Util.Run (safeSpawnProg)

myModMask :: KeyMask
myModMask = mod4Mask

myLauncher :: String
myLauncher = "dmenu_run -i -l 15 -p '➤' -nb '#282828' -nf '#ebdbb2' -sb '#8ec07c' -sf '#282828'"

myKeys :: (XConfig Layout -> Map.Map (ButtonMask, KeySym) (X ()))
myKeys conf =
  let m = modMask conf
   in Map.fromList $
        -- {{{2 Programs
        [ ((myModMask, xK_p), spawn myLauncher),
          ((myModMask .|. shiftMask, xK_Return), runCommand myTerminalCommand),
          ((myModMask .|. shiftMask, xK_c), kill),
          -- {{{2 XMonad
          ((myModMask, xK_q), restart "xmonad" True),
          ((myModMask .|. shiftMask, xK_q), io exitSuccess),
          -- {{{2 Layouts
          ((myModMask, xK_space), sendMessage NextLayout),
          -- Reset the layouts on the current workspace
          ((myModMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
          -- {{{2 Focus
          -- Move focus to the next window
          ((myModMask, xK_Tab), windows StackSet.focusDown),
          ((myModMask, xK_j), windows StackSet.focusDown),
          ((myModMask, xK_k), windows StackSet.focusUp),
          ((myModMask, xK_m), windows StackSet.focusMaster),
          -- Swap the focused window with the next window
          ((myModMask .|. shiftMask, xK_j), windows StackSet.swapDown),
          -- Swap the focused window with the previous window
          ((myModMask .|. shiftMask, xK_k), windows StackSet.swapUp),
          -- Swap the focused window and the master window
          ((myModMask, xK_Return), windows StackSet.swapMaster),
          -- {{{2 Sizing
          -- Increment the number of windows in the master area
          ((myModMask, xK_comma), sendMessage (IncMasterN 1)),
          -- Deincrement the number of windows in the master area
          ((myModMask, xK_period), sendMessage (IncMasterN (-1))),
          -- Resize viewed windows to the correct size
          ((myModMask, xK_n), refresh),
          -- Shrink the master area
          ((myModMask, xK_h), sendMessage Shrink),
          -- Expand the master area
          ((myModMask, xK_l), sendMessage Expand),
          -- Push window back into tiling
          ((myModMask, xK_t), withFocused $ windows . StackSet.sink),
          ((myModMask, xK_b), sendMessage ToggleStruts),
          -- {{{2 Screenshots
          ((0, xK_Print), spawn "printscreen -a"),
          ((myModMask, xK_Print), spawn "printscreen -s"),
          ((myModMask .|. shiftMask, xK_Print), spawn "printscreen -c"),
          -- {{{2 Audio Controls
          ((0, xF86XK_AudioMute), runCommand . PVolume.toggle $ myVolumeControl),
          ((0, xF86XK_AudioLowerVolume), runCommand . PVolume.lower myVolumeControl $ 5),
          ((0, xF86XK_AudioRaiseVolume), runCommand . PVolume.raise myVolumeControl $ 5),
          -- {{{2 Music Controls
          ((0, xF86XK_AudioNext), runCommand . nextSong $ myMusicPlayer),
          ((0, xF86XK_AudioPrev), runCommand . previousSong $ myMusicPlayer),
          ((0, xF86XK_AudioStop), runCommand . stop $ myMusicPlayer),
          ((0, xF86XK_AudioPlay), runCommand . toggle $ myMusicPlayer),
          -- {{{2 Brightness Controls
          ((0, xF86XK_MonBrightnessDown), PBrightness.update =<< decreaseBrightness),
          ((0, xF86XK_MonBrightnessUp), PBrightness.update =<< increaseBrightness),
          -- {{{2 Screens Related
          ((myModMask, xK_Left), prevScreen),
          ((myModMask .|. shiftMask, xK_Left), shiftPrevScreen),
          ((myModMask, xK_Right), nextScreen),
          ((myModMask .|. shiftMask, xK_Right), shiftNextScreen),
          -- {{{2 Search Engines
          ((myModMask, xK_s), Submap.submap $ searchEngineMap $ Search.promptSearch myPrompt),
          ((myModMask .|. shiftMask, xK_s), Submap.submap $ searchEngineMap Search.selectSearch),
          -- {{{2 Applications
          ((myModMask, xK_o), Submap.submap applicationsSpawn),
          -- {{2 Grid Select
          ((myModMask, xK_a), Submap.submap gridMap),
          -- {{{2 Urgency Hooks
          ((myModMask, xK_u), focusUrgent),
          ((myModMask .|. shiftMask, xK_u), clearUrgents)
        ]
          ++ [ ((m .|. e .|. i, key), windows (onCurrentScreen f workspace))
               | (key, workspace) <- zip [xK_1 .. xK_9] (workspaces' conf),
                 (e, f) <- [(0, StackSet.view), (shiftMask, viewShift)],
                 i <- [0, controlMask, myModMask, controlMask .|. myModMask]
             ]
          ++ [ ((myModMask .|. mask, key), f sc)
               | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
                 (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]
             ]
  where
    viewShift i = StackSet.view i . StackSet.shift i
    searchEngineMap method =
      Map.fromList
        [ ((0, xK_a), method Search.amazon),
          ((0, xK_h), method Search.hoogle),
          ((0, xK_i), method Search.images),
          ((0, xK_m), method Search.imdb),
          ((0, xK_s), method Search.stackage),
          ((0, xK_w), method Search.wikipedia),
          ((0, xK_y), method Search.youtube),
          ( (0, xK_g),
            method
              ( Search.intelligent Search.wikipedia
                  !> Search.hoogle
                  !> Search.stackage
                  !> Search.youtube
                  !> Search.prefixAware Search.duckduckgo
              )
          )
        ]
    applicationsSpawn =
      Map.fromList
        [ ((0, xK_e), runCommand muttCommand),
          ((0, xK_n), runCommand newsboatCommand),
          ((0, xK_c), runCommand weechatCommand),
          ((0, xK_m), runCommand sptCommand),
          ((0, xK_f), runCommand rangerCommand),
          ((0, xK_b), catchIO . safeSpawnProg $ "firefox"),
          ((0, xK_v), catchIO . safeSpawnProg $ "zathura"),
          ((0, xK_i), catchIO . safeSpawnProg $ "krita")
        ]
    gridMap =
      Map.fromList
        [ ((0, xK_s), goToSelected myGridConfig),
          ((0, xK_g), windowPrompt myPrompt Goto allWindows)
        ]
    decreaseBrightness = PBrightness.decreaseBy <$> PBrightness.getDevice <*> pure percent
    increaseBrightness = PBrightness.increaseBy <$> PBrightness.getDevice <*> pure percent
    percent = fromJust . PBrightness.percent $ 5
