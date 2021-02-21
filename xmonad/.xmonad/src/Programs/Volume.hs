{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Programs.Volume
  ( VolumeControls (..),
    alsaControl,
    myVolumeControl,
    getDevice,
    writeDevice,
  )
where

import Control.Lens
  ( makeLenses,
    (&),
    (.~),
    (^.),
  )
import Control.Monad (void)
import Control.Monad.Except (ExceptT (..), liftEither)
import Control.Monad.Trans.Maybe (MaybeT (..))
import DBus
  ( Address,
    MethodCall (..),
    ObjectPath,
    fromVariant,
    methodCall,
    methodReturnBody,
    parseAddress,
    toVariant,
  )
import DBus.Client
  ( Client,
    callNoReply,
    call_,
    connect,
    connectSession,
    getPropertyValue,
  )
import DBus.Notify
  ( Body (..),
    Hint (Urgency),
    Note (..),
    UrgencyLevel (Low),
    blankNote,
    connectSession,
    notify,
  )
import Data.List (genericLength)
import Data.Word (Word32)
import Programs.Commands
  ( Command,
    createCommand,
  )
import XMonad
  ( MonadIO (..),
  )
import qualified XMonad.Actions.Volume as AlsaMixer
import XMonad.Core (catchIO)

data VolumeControls m = VolumeControls
  { toggle :: Command m,
    raise :: Double -> Command m,
    lower :: Double -> Command m
  }

newtype Percent
  = Percent Integer
  deriving (Show, Eq, Ord)

data PulseAudioDevice = PulseAudioDevice
  { _socket :: Address,
    _sink :: ObjectPath,
    _volume :: Word32,
    _muted :: Bool
  }
  deriving (Show, Eq)

makeLenses ''PulseAudioDevice

myVolumeControl :: MonadIO m => VolumeControls m
myVolumeControl = alsaControl

alsaControl :: MonadIO m => VolumeControls m
alsaControl =
  VolumeControls
    { toggle = createCommand $ void AlsaMixer.toggleMute,
      raise = createCommand . void . AlsaMixer.raiseVolume,
      lower = createCommand . void . AlsaMixer.lowerVolume
    }

-- TODO: Use the defined command to raise volume with PulseAudio
pulseaudioControl :: MonadIO m => VolumeControls m
pulseaudioControl =
  VolumeControls
    { toggle = createCommand $ void AlsaMixer.toggleMute,
      raise = createCommand . void . AlsaMixer.raiseVolume,
      lower = createCommand . void . AlsaMixer.lowerVolume
    }

writeDevice :: MonadIO m => PulseAudioDevice -> m ()
writeDevice device' =
  pure ()

getDevice :: MonadIO m => MaybeT m PulseAudioDevice
getDevice = do
  address <- pulseAudioAddress
  client <- liftIO $ connect address
  sink' <- defaultSink client
  PulseAudioDevice address sink' <$> getVolume client sink' <*> isMuted client sink'

toggleMute :: PulseAudioDevice -> PulseAudioDevice
toggleMute device' = device' & muted .~ (not muted')
  where
    muted' :: Bool
    muted' = device' ^. muted

defaultSink :: MonadIO m => Client -> MaybeT m ObjectPath
defaultSink client = MaybeT $ do
  value <- liftIO $ call_ client method
  case methodReturnBody value of
    [] -> pure Nothing
    (extracted : _) -> pure . fromVariant $ extracted
  where
    method =
      (methodCall "/org/pulseaudio/core1" "org.PulseAudio.Core1" "GetSinkByName")
        { methodCallDestination = Just "org.PulseAudio1"
        }

isMuted :: MonadIO m => Client -> ObjectPath -> MaybeT m Bool
isMuted client sink' = MaybeT $ do
  value <- liftIO $ getPropertyValue client method
  case value of
    Left _ -> pure Nothing
    Right extracted -> pure . Just $ extracted
  where
    method =
      (methodCall sink' "org.PulseAudio.Core1.Device" "Mute")
        { methodCallDestination = Just "org.PulseAudio1"
        }

getVolume :: MonadIO m => Client -> ObjectPath -> MaybeT m Word32
getVolume client sink' = MaybeT $ do
  value <- liftIO $ getPropertyValue client method
  case value of
    Left _ -> pure Nothing
    Right extracted -> pure . Just . fromInteger . average $ extracted
  where
    method =
      (methodCall sink' "org.PulseAudio.Core1.Device" "Volume")
        { methodCallDestination = Just "org.PulseAudio1"
        }

    average :: [Word32] -> Integer
    average [] = 0
    average xs = floor $ realToFrac (sum xs) / genericLength xs

pulseAudioAddress :: MonadIO m => MaybeT m Address
pulseAudioAddress =
  MaybeT $ do
    session <- liftIO connectSession
    value <- liftIO $ getPropertyValue session method
    case parseAddress <$> value of
      Left _ -> pure Nothing
      Right parsed -> pure parsed
  where
    method = methodCall "/org/pulseaudio/server_lookup1" "org.PulseAudio.ServerLookup1" "Address"

-- TODO: Handle notifications for mute/unmute
createNote :: Word32 -> Note
createNote volume' =
  blankNote
    { summary = "Volume",
      appName = "XMonad",
      body = Just . Text $ show volume' ++ "%",
      hints = [Urgency Low]
    }

sendNotification :: MonadIO m => Note -> m ()
sendNotification note = do
  client <- liftIO connectSession
  catchIO . void $ notify client note
