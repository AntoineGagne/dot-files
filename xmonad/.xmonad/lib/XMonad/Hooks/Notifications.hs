module XMonad.Hooks.Notifications
    ( LibNotifyUrgencyHook (..)
    ) where

import XMonad ( gets
              , windowset
              )
import XMonad.Hooks.UrgencyHook ( UrgencyHook (..) )
import XMonad.Util.NamedWindows ( getName )
import XMonad.Util.Run ( safeSpawn )

import qualified XMonad.StackSet as StackSet

data LibNotifyUrgencyHook = LibNotifyUrgencyHook
    deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook window = do
        name <- getName window
        Just workspaceId <- StackSet.findTag window <$> gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ workspaceId]
