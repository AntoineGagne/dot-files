module Programs.Volume
  ( VolumeControls (..),
    alsaControl,
    myVolumeControl,
  )
where

import Control.Monad (void)
import DBus.Notify
  ( Body (..),
    Hint (Urgency),
    Note (..),
    UrgencyLevel (Low),
    blankNote,
    connectSession,
    notify,
  )
import Data.Foldable (traverse_)
import Programs.Commands
  ( Command,
    createCommand,
  )
import Sound.ALSA.Mixer
  ( Channel (FrontLeft),
    Volume (..),
    channels,
    getChannel,
    getControlByName,
    getRange,
    playback,
    setChannel,
    switch,
    volume,
    withMixer,
  )
import XMonad
  ( MonadIO (..),
  )
import XMonad.Core (catchIO)

data VolumeControls m = VolumeControls
  { toggle :: Command m,
    raise :: Double -> Command m,
    lower :: Double -> Command m
  }

myVolumeControl :: MonadIO m => VolumeControls m
myVolumeControl = alsaControl

-- | Control ALSA @default@ mixer.
alsaControl :: MonadIO m => VolumeControls m
alsaControl =
  VolumeControls
    { toggle = createCommand . void . liftIO $ toggleMute,
      raise = createCommand . void . liftIO . increaseBy,
      lower = createCommand . void . liftIO . decreaseBy
    }

toggleMute :: IO ()
toggleMute =
  withMixer "default" $ \mixer ->
    do
      Just control <- getControlByName mixer "Master"
      let Just playbackSwitch = playback $ switch control
      Just switch' <- getChannel FrontLeft playbackSwitch
      setChannel FrontLeft playbackSwitch $ not switch'
      sendNotification (muteNote switch')

increaseBy :: Double -> IO ()
increaseBy n = changeVolumeWith (+ floor n)

decreaseBy :: Double -> IO ()
decreaseBy n = changeVolumeWith (flip (-) (floor n))

changeVolumeWith :: (Integer -> Integer) -> IO ()
changeVolumeWith operation =
  withMixer "default" $ \mixer ->
    do
      Just control <- getControlByName mixer "Master"
      let Just playbackVolume = playback $ volume control
      (minimum', maximum') <- getRange playbackVolume
      traverse_
        ( \channel -> do
            Just current <- getChannel channel $ value playbackVolume
            let current' = toInteger current
                minimum'' = toInteger minimum'
                maximum'' = toInteger maximum'
                percentage = operation $ currentPercentage current' maximum''
                new = setPercentage percentage minimum'' maximum''
             in setChannel channel (value playbackVolume) (fromInteger new)
        )
        $ channels (value playbackVolume)

currentPercentage :: Integer -> Integer -> Integer
currentPercentage current maximum' = floor $ current' / maximum'' * 100
  where
    current' :: Double
    current' = fromInteger current

    maximum'' :: Double
    maximum'' = fromInteger maximum'

setPercentage :: Integer -> Integer -> Integer -> Integer
setPercentage n minimum' maximum' = floor $ normalizeValue computed
  where
    computed :: Double
    computed = fromInteger n / 100 * maximum''

    normalizeValue :: Double -> Double
    normalizeValue = max minimum'' . min maximum''

    minimum'' :: Double
    minimum'' = fromInteger minimum'

    maximum'' :: Double
    maximum'' = fromInteger maximum'

muteNote :: Bool -> Note
muteNote muted =
  note
    { body = Just . Text $ if muted then "Mute" else "Unmute"
    }

note :: Note
note =
  blankNote
    { summary = "Volume",
      appName = "XMonad",
      hints = [Urgency Low]
    }

sendNotification :: MonadIO m => Note -> m ()
sendNotification note' = do
  client <- liftIO connectSession
  catchIO . void $ notify client note'
