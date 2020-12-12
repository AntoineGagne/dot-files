{-# LANGUAGE TemplateHaskell #-}

module Programs.Brightness
    ( increaseBy
    , decreaseBy
    , currentPercentage
    , getDevice
    , update
    , percent
    , Percent
    , BrightnessDevice

    , dummyDevice
    ) where


import Control.Monad ( void )

import Control.Lens
    ( makeLenses
    , (^.)
    , (.~)
    , (&)
    )
import XMonad ( MonadIO (..) )
import XMonad.Core ( catchIO )
import DBus.Notify ( blankNote
                   , connectSession
                   , notify
                   , Body (..)
                   , Note (..)
                   , Hint ( Urgency )
                   , UrgencyLevel ( Low )
                   )
import System.EasyFile
    ( (</>)
    )

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

newtype Percent
    = Percent Integer
    deriving (Show, Eq, Ord)

data BrightnessDevice = BrightnessDevice
    { _actualBrightness :: Integer
    , _maximumBrightness :: Integer
    , _brightness :: Integer
    , _device :: FilePath
    } deriving (Show, Eq)
makeLenses ''BrightnessDevice

getDevice :: MonadIO m => m BrightnessDevice
getDevice = readDevice "/sys/class/backlight/intel_backlight"

update :: MonadIO m => BrightnessDevice -> m ()
update device' = do
    _ <- writeDevice device'
    latest <- getDevice
    sendNotification . createNote . fromInteger $ currentPercentage latest

percent :: Integer -> Maybe Percent
percent n
    | n < 0 = Nothing
    | n > 100 = Nothing
    | otherwise = Just . Percent $ n

increaseBy :: BrightnessDevice -> Percent -> BrightnessDevice
increaseBy device' (Percent n) = changeWith device' (+ n)

decreaseBy :: BrightnessDevice -> Percent -> BrightnessDevice
decreaseBy device' (Percent n) = changeWith device' (flip (-) n)

changeWith :: BrightnessDevice -> (Integer -> Integer) -> BrightnessDevice
changeWith device' operation = setPercentage device' new
    where
        new = operation $ currentPercentage device'

currentPercentage :: BrightnessDevice -> Integer
currentPercentage device' = floor $ actual / maximum' * 100
    where
        actual :: Double
        actual = fromInteger (device'^.actualBrightness )

        maximum' :: Double
        maximum' = fromInteger (device'^.maximumBrightness)

setPercentage :: BrightnessDevice -> Integer -> BrightnessDevice
setPercentage device' n = device' & brightness .~ new
  where
    new :: Integer
    new = floor $ normalizeValue maximum' computed

    computed :: Double
    computed = fromInteger n / 100 * maximum'

    maximum' :: Double
    maximum' = fromInteger (device'^.maximumBrightness)

    normalizeValue :: Double -> Double -> Double
    normalizeValue maximum'' = max 0 . min maximum''

writeDevice :: MonadIO m => BrightnessDevice -> m ()
writeDevice device' = liftIO $ TextIO.writeFile path new
    where
        path = device'^.device </> "brightness"
        new = Text.pack . show $ device'^.brightness

readDevice :: MonadIO m => FilePath -> m BrightnessDevice
readDevice devicePath = liftIO $
    BrightnessDevice
        <$> readIntegerFromFile "actual_brightness"
        <*> readIntegerFromFile "max_brightness"
        <*> readIntegerFromFile "brightness"
        <*> pure devicePath
  where
      readIntegerFromFile path = read . Text.unpack <$> TextIO.readFile (devicePath </> path)

createNote :: Int -> Note
createNote brightnessLevel = blankNote
    { summary = "Brightness"
    , appName = "XMonad"
    , body = Just . Text $ show brightnessLevel ++ "%"
    , hints = [Urgency Low]
    }

sendNotification :: MonadIO m => Note -> m ()
sendNotification note = do
    client <- liftIO connectSession
    catchIO . void $ notify client note

dummyDevice :: Integer -> Integer -> Integer -> BrightnessDevice
dummyDevice actual brightness' maximum' =
    BrightnessDevice
        { _actualBrightness = actual
        , _brightness = brightness'
        , _maximumBrightness = maximum'
        , _device = ""
        }
