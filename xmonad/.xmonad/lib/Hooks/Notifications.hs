{-# LANGUAGE FlexibleContexts #-}

module Hooks.Notifications
  ( LibNotifyUrgencyHook (..),
    myUrgencyHook,
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
import Data.Maybe
  ( Maybe,
    fromMaybe,
  )
import XMonad
  ( MonadIO (..),
    Window,
    XConfig,
    gets,
    windowset,
  )
import XMonad.Core
  ( LayoutClass,
    catchIO,
  )
import XMonad.Hooks.DynamicLog
  ( dzenStrip,
    xmobarStrip,
  )
import XMonad.Hooks.UrgencyHook
  ( RemindWhen (..),
    SuppressWhen (..),
    UrgencyConfig (..),
    UrgencyHook (..),
    minutes,
    urgencyConfig,
    withUrgencyHookC,
  )
import XMonad.StackSet (findTag)
import XMonad.Util.NamedWindows
  ( NamedWindow,
    getName,
  )

myUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
myUrgencyHook =
  withUrgencyHookC
    LibNotifyUrgencyHook
    urgencyConfig
      { suppressWhen = Visible,
        remindWhen = Every (minutes 5.0)
      }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook window = do
    name <- getName window
    workspaceId <- findTag window <$> gets windowset
    sendNotification $ createNote name workspaceId

createNote :: Show i => NamedWindow -> Maybe i -> Note
createNote namedWindow workspaceId =
  blankNote
    { summary = "Workspace: " ++ fromMaybe "n/a" workspace,
      appName = "Notification",
      body = Just . Text . show $ namedWindow,
      hints = [Urgency Low]
    }
  where
    workspace = dzenStrip . xmobarStrip . show <$> workspaceId

sendNotification :: MonadIO m => Note -> m ()
sendNotification note = do
  client <- liftIO connectSession
  catchIO . void $ notify client note
