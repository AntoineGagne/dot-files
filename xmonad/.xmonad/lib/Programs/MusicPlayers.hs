module Programs.MusicPlayers
    ( myMusicPlayer
    , mpd
    , MusicPlayerControls (..)
    ) where

import Control.Monad ( void )
import Network.MPD ( withMPD
                   , MPD
                   , Response
                   )
import Network.MPD.Applicative ( runCommand )
import XMonad ( MonadIO (..) )

import Programs.Commands ( Command
                         , createCommand
                         )

import qualified Network.MPD.Applicative as MPDApplicative
import qualified Network.MPD.Applicative.PlaybackControl as PlaybackControl
import qualified Network.MPD.Commands.Extensions as MPDExtensions

data MusicPlayerControls m = MusicPlayerControls
    { toggle :: Command m
    , stop :: Command m
    , nextSong :: Command m
    , previousSong :: Command m
    }

myMusicPlayer :: MonadIO m => MusicPlayerControls m
myMusicPlayer = mpd

mpd :: MonadIO m => MusicPlayerControls m
mpd = MusicPlayerControls
    { toggle = createCommand . liftMPD_ $ MPDExtensions.toggle
    , stop = runMPDCommand PlaybackControl.stop
    , nextSong = runMPDCommand PlaybackControl.next
    , previousSong = runMPDCommand PlaybackControl.previous
    }

runMPDCommand :: MonadIO m => MPDApplicative.Command a -> Command m
runMPDCommand = createCommand . liftMPD_ . runCommand

-- Taken shamelessly from:
-- https://github.com/Fuco1/xmonad-config
liftMPD_ :: MonadIO m => MPD a -> m ()
liftMPD_ = void . liftMPD

liftMPD :: MonadIO m => MPD a -> m (Response a)
liftMPD = liftIO . withMPD
