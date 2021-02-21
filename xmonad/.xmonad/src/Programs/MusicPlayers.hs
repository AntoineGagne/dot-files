{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Programs.MusicPlayers
  ( myMusicPlayer,
    mpd,
    spotify,
    MusicPlayerControls (..),
  )
where

import Control.Monad (void)
import DBus
  ( MethodCall (..),
    methodCall,
  )
import DBus.Client
  ( callNoReply,
    connectSession,
  )
#ifdef WITH_MPD
import Network.MPD
  ( MPD,
    Response,
    withMPD,
  )
import Network.MPD.Applicative (runCommand)
import qualified Network.MPD.Applicative as MPDApplicative
import qualified Network.MPD.Applicative.PlaybackControl as PlaybackControl
#endif
import Programs.Commands
  ( Command,
    createCommand,
  )
import XMonad (MonadIO (..))

data MusicPlayerControls m = MusicPlayerControls
  { toggle :: Command m,
    stop :: Command m,
    nextSong :: Command m,
    previousSong :: Command m
  }

myMusicPlayer :: MonadIO m => MusicPlayerControls m
myMusicPlayer = spotifyd

spotify :: MonadIO m => MusicPlayerControls m
spotify =
  MusicPlayerControls
    { toggle = createCommand . callMethod $ method "PlayPause",
      stop = createCommand . callMethod $ method "Stop",
      nextSong = createCommand . callMethod $ method "Next",
      previousSong = createCommand . callMethod $ method "Previous"
    }
  where
    method name =
      (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" name)
        { methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }

spotifyd :: MonadIO m => MusicPlayerControls m
spotifyd =
  MusicPlayerControls
    { toggle = createCommand . callMethod $ method "PlayPause",
      stop = createCommand . callMethod $ method "Stop",
      nextSong = createCommand . callMethod $ method "Next",
      previousSong = createCommand . callMethod $ method "Previous"
    }
  where
    method name =
      (methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" name)
        { methodCallDestination = Just "org.mpris.MediaPlayer2.spotifyd"
        }

callMethod :: MonadIO m => MethodCall -> m ()
callMethod method = do
  client <- liftIO connectSession
  liftIO $ callNoReply client method

#ifdef WITH_MPD
mpd :: MonadIO m => MusicPlayerControls m
mpd =
  MusicPlayerControls
    { toggle = runMPDCommand PlaybackControl.toggle,
      stop = runMPDCommand PlaybackControl.stop,
      nextSong = runMPDCommand PlaybackControl.next,
      previousSong = runMPDCommand PlaybackControl.previous
    }

runMPDCommand :: MonadIO m => MPDApplicative.Command a -> Command m
runMPDCommand = createCommand . liftMPD_ . runCommand

-- Taken shamelessly from:
-- https://github.com/Fuco1/xmonad-config
liftMPD_ :: MonadIO m => MPD a -> m ()
liftMPD_ = void . liftMPD

liftMPD :: MonadIO m => MPD a -> m (Response a)
liftMPD = liftIO . withMPD
#endif
