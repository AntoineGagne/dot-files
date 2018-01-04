module XMonad.Hooks.Notifications
    ( LibNotifyUrgencyHook (..)
    ) where

import Control.Monad ( void )
import Data.Maybe ( Maybe
                  , fromMaybe
                  )
import DBus.Notify ( blankNote
                   , connectSession
                   , notify
                   , Body (..)
                   , Client
                   , Note (..)
                   , Hint ( Urgency )
                   , UrgencyLevel ( Low )
                   )
import XMonad ( gets
              , windowset
              , MonadIO (..)
              )
import XMonad.Core ( catchIO )
import XMonad.Hooks.DynamicLog ( dzenStrip
                               , xmobarStrip
                               )
import XMonad.Hooks.UrgencyHook ( UrgencyHook (..) )
import XMonad.Layout.IndependentScreens ( unmarshallS
                                        , unmarshallW
                                        )
import XMonad.StackSet ( findTag )
import XMonad.Util.NamedWindows ( getName
                                , NamedWindow (..)
                                )
import XMonad.Util.Run ( safeSpawn )

data LibNotifyUrgencyHook = LibNotifyUrgencyHook
    deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook window = do
        name <- getName window
        workspaceId <- findTag window <$> gets windowset
        sendNotification $ createNote name workspaceId

createNote :: Show i => NamedWindow -> Maybe i -> Note
createNote namedWindow workspaceId = blankNote
    { summary = "Workspace: " ++ fromMaybe "n/a" workspace
    , appName = "Notification"
    , body = Just . Text . show $ namedWindow
    , hints = [Urgency Low]
    }
        where
            workspace = dzenStrip . xmobarStrip . show <$> workspaceId

sendNotification :: MonadIO m => Note -> m ()
sendNotification note = do
    client <- liftIO connectSession
    catchIO . void $ notify client note
