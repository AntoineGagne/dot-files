module Programs.Volume
    ( VolumeControls (..)
    , alsaControl
    , scriptControl
    , myVolumeControl
    ) where

import Control.Monad ( void )
import XMonad ( MonadIO (..)
              , spawn
              )
import XMonad.Actions.Volume ( raiseVolume
                             , lowerVolume
                             , toggleMute
                             )

import Programs.Commands ( Command
                         , createCommand
                         )

data VolumeControls m = VolumeControls
    { toggle :: Command m
    , raise :: Double -> Command m
    , lower :: Double -> Command m
    }

myVolumeControl :: MonadIO m => VolumeControls m
myVolumeControl = scriptControl

alsaControl :: MonadIO m => VolumeControls m
alsaControl = VolumeControls
    { toggle = createCommand $ void toggleMute
    , raise = createCommand . void . raiseVolume
    , lower = createCommand . void . lowerVolume
    }

scriptControl :: MonadIO m => VolumeControls m
scriptControl = VolumeControls
    { toggle = createCommand $ spawn "control-volume -t"
    , raise = \_ -> createCommand . void . spawn $ "control-volume -c +5%"
    , lower = \_ -> createCommand . void . spawn $ "control-volume -c -5%"
    }
