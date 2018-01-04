{-# LANGUAGE RecordWildCards #-}

module XMonad.Programs.Terminals
    ( TerminalEmulator (..)
    , kitty
    , urxvt
    , mutt
    , muttCommand
    , myTerminal
    , ncmpcpp
    , ncmpcppCommand
    , newsboat
    , newsboatCommand
    , weechat
    , weechatCommand
    ) where

import XMonad ( MonadIO )
import XMonad.Core ( spawn )

import XMonad.Programs.Commands ( Command
                                , createCommand
                                )

data TerminalEmulator = TerminalEmulator
    { terminalName :: String
    , terminalDaemonName :: String
    , terminalTitleOption :: String
    , terminalExecutionOption :: String
    }

instance Show TerminalEmulator where
    show TerminalEmulator { terminalDaemonName = terminalDaemonName' } = terminalDaemonName'

myTerminal :: TerminalEmulator
myTerminal = urxvt

kitty :: TerminalEmulator
kitty = TerminalEmulator
    { terminalName = "kitty"
    , terminalDaemonName = "kitty --single-instance"
    , terminalTitleOption = "--class="
    , terminalExecutionOption = " "
    }

urxvt :: TerminalEmulator
urxvt = TerminalEmulator
    { terminalName = "urxvt"
    , terminalDaemonName = "urxvtc"
    , terminalTitleOption = "-title "
    , terminalExecutionOption = "-e"
    }

data TerminalProgram = TerminalProgram
    { programName :: String
    , programTitle :: String
    , programCommand :: String
    , programType :: String
    }

muttCommand :: MonadIO m => Command m
muttCommand = createProgramCommand mutt

ncmpcppCommand :: MonadIO m => Command m
ncmpcppCommand = createProgramCommand ncmpcpp

newsboatCommand :: MonadIO m => Command m
newsboatCommand = createProgramCommand newsboat

weechatCommand :: MonadIO m => Command m
weechatCommand = createProgramCommand weechat

createProgramCommand :: MonadIO m => TerminalProgram -> Command m
createProgramCommand = createCommand . spawn . launchApp myTerminal

mutt :: TerminalProgram
mutt = TerminalProgram
    { programName = "mutt"
    , programTitle = "mutt"
    , programCommand = "mutt"
    , programType = "email"
    }

ncmpcpp :: TerminalProgram
ncmpcpp = TerminalProgram
    { programName = "ncmpcpp"
    , programTitle = "ncmpcpp"
    , programCommand = "ncmpcpp"
    , programType = "music"
    }

newsboat :: TerminalProgram
newsboat = TerminalProgram
    { programName = "newsboat"
    , programTitle = "newsboat"
    , programCommand = "newsboat"
    , programType = "news"
    }

weechat :: TerminalProgram
weechat = TerminalProgram
    { programName = "weechat"
    , programTitle = "weechat"
    , programCommand = "weechat"
    , programType = "chat"
    }

launchApp :: TerminalEmulator -> TerminalProgram -> String
launchApp TerminalEmulator {..} TerminalProgram {..} = terminalDaemonName
                                             ++ " "
                                             ++ terminalTitleOption
                                             ++ "'"
                                             ++ programTitle
                                             ++ "' "
                                             ++ terminalExecutionOption
                                             ++ " "
                                             ++ launchCommand
    where launchCommand = "bash -c 'tmux -q has-session -t "
                        ++ programType
                        ++ " && tmux -2 attach-session -t "
                        ++ programType
                        ++ " || "
                        ++ programName
                        ++ "'"
