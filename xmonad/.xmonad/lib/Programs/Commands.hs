module Programs.Commands
    ( Command
    , createCommand
    , runCommand
    ) where

import XMonad ( MonadIO )

newtype Command m = Command (m ())

runCommand :: MonadIO m => Command m -> m ()
runCommand (Command command) = command

createCommand :: MonadIO m => m () -> Command m
createCommand = Command
