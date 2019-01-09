module Programs.MusicPlayers
    ( myMusicPlayer
    , mpd
    , spotify
    , MusicPlayerControls (..)
    ) where

import Control.Monad ( void )
import Network.MPD ( withMPD
                   , MPD
                   , Response
                   )
import Network.MPD.Applicative ( runCommand )
import XMonad ( MonadIO (..) )
import XMonad.Core ( spawn )

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
myMusicPlayer = spotify

mpd :: MonadIO m => MusicPlayerControls m
mpd = MusicPlayerControls
    { toggle = createCommand . liftMPD_ $ MPDExtensions.toggle
    , stop = runMPDCommand PlaybackControl.stop
    , nextSong = runMPDCommand PlaybackControl.next
    , previousSong = runMPDCommand PlaybackControl.previous
    }

spotify :: MonadIO m => MusicPlayerControls m
spotify = MusicPlayerControls
    { toggle = createCommand . spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"
    , stop = createCommand . spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop"
    , nextSong = createCommand . spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"
    , previousSong = createCommand . spawn $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"
    }

runMPDCommand :: MonadIO m => MPDApplicative.Command a -> Command m
runMPDCommand = createCommand . liftMPD_ . runCommand

-- Taken shamelessly from:
-- https://github.com/Fuco1/xmonad-config
liftMPD_ :: MonadIO m => MPD a -> m ()
liftMPD_ = void . liftMPD

liftMPD :: MonadIO m => MPD a -> m (Response a)
liftMPD = liftIO . withMPD
