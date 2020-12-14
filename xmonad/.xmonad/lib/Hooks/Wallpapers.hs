module Hooks.Wallpapers
  ( myWallpaperSetterHook,
  )
where

import XMonad
  ( ScreenId,
    X,
  )
import XMonad.Hooks.ManageHooks (myWorkspaces)
import XMonad.Hooks.WallpaperSetter
  ( Wallpaper (WallpaperDir),
    WallpaperConf (..),
    WallpaperList (..),
    defWPNames,
    defWallpaperConf,
    wallpaperSetter,
  )
import XMonad.Layout.IndependentScreens (marshall)

-- TODO: Find a way to make it work with the multi-head setup
myWallpaperSetterHook :: ScreenId -> X ()
myWallpaperSetterHook screenNumber =
  wallpaperSetter
    defWallpaperConf
      { wallpapers = WallpaperList $ zip marshalledWorkspaces $ replicate (length myWorkspaces) (WallpaperDir "~/.wallpapers/"),
        wallpaperBaseDir = "~/.wallpapers/"
      }
  where
    marshalledWorkspaces = [marshall id marshalledWorkspace | marshalledWorkspace <- myWorkspaces, id <- [0 .. screenNumber - 1]]
