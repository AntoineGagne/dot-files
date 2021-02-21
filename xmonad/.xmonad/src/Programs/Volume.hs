module Programs.Volume
  ( VolumeControls (..),
    alsaControl,
    scriptControl,
    myVolumeControl,
  )
where

import Control.Monad (void)
import Programs.Commands
  ( Command,
    createCommand,
  )
import XMonad
  ( MonadIO (..),
    spawn,
  )
import XMonad.Actions.Volume
  ( lowerVolume,
    raiseVolume,
    toggleMute,
  )

data VolumeControls m = VolumeControls
  { toggle :: Command m,
    raise :: Double -> Command m,
    lower :: Double -> Command m
  }

myVolumeControl :: MonadIO m => VolumeControls m
myVolumeControl = alsaControl

alsaControl :: MonadIO m => VolumeControls m
alsaControl =
  VolumeControls
    { toggle = createCommand $ void toggleMute,
      raise = createCommand . void . raiseVolume,
      lower = createCommand . void . lowerVolume
    }
