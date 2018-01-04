module XMonad.Hooks.Wallpapers
    ( myWallpaperSetterHook
    ) where

import XMonad ( ScreenId
              , X
              )
import XMonad.Layout.IndependentScreens ( marshall )
import XMonad.Hooks.WallpaperSetter ( defWallpaperConf
                                    , defWPNames
                                    , wallpaperSetter
                                    , WallpaperConf (..)
                                    , Wallpaper ( WallpaperDir )
                                    , WallpaperList (..)
                                    )

import XMonad.Hooks.ManageHooks ( myWorkspaces )

-- TODO: Find a way to make it work with the multi-head setup
myWallpaperSetterHook :: ScreenId -> X ()
myWallpaperSetterHook screenNumber = wallpaperSetter defWallpaperConf
    { wallpapers = WallpaperList $ zip marshalledWorkspaces $ replicate (length myWorkspaces) (WallpaperDir "~/.wallpapers/")
    , wallpaperBaseDir = "~/.wallpapers/"
    }
        where marshalledWorkspaces = [marshall id marshalledWorkspace | marshalledWorkspace <- myWorkspaces, id <- [0..screenNumber - 1]]
