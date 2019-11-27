{-# LANGUAGE RecordWildCards #-}

module Programs.Terminals
    ( TerminalEmulator (..)
    , kitty
    , alacritty
    , urxvt
    , mutt
    , muttCommand
    , myTerminal
    , myTerminalCommand
    , ncmpcpp
    , ncmpcppCommand
    , newsboat
    , newsboatCommand
    , weechat
    , weechatCommand
    , ranger
    , rangerCommand
    , spt
    , sptCommand
    ) where

import XMonad ( MonadIO )
import XMonad.Core ( spawn )

import Programs.Commands ( Command
                         , createCommand
                         )

data TerminalEmulator = TerminalEmulator
    { terminalName :: String
    , terminalDaemonName :: String
    , terminalTitleOption :: String
    , terminalExecutionOption :: String
    }

myTerminal :: TerminalEmulator
myTerminal = alacritty

myTerminalCommand :: MonadIO m => Command m
myTerminalCommand = createCommand . spawn . terminalDaemonName $ myTerminal

kitty :: TerminalEmulator
kitty = TerminalEmulator
    { terminalName = "kitty"
    , terminalDaemonName = "kitty --single-instance"
    , terminalTitleOption = "--class="
    , terminalExecutionOption = " "
    }

alacritty :: TerminalEmulator
alacritty = TerminalEmulator
    { terminalName = "alacritty"
    , terminalDaemonName = "alacritty"
    , terminalTitleOption = "--title "
    , terminalExecutionOption = "--command "
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

sptCommand :: MonadIO m => Command m
sptCommand = createProgramCommand spt

newsboatCommand :: MonadIO m => Command m
newsboatCommand = createProgramCommand newsboat

weechatCommand :: MonadIO m => Command m
weechatCommand = createProgramCommand weechat

rangerCommand :: MonadIO m => Command m
rangerCommand = createProgramCommand ranger

createProgramCommand :: MonadIO m => TerminalProgram -> Command m
createProgramCommand = createCommand . spawn . launchApp myTerminal

mutt :: TerminalProgram
mutt = TerminalProgram
    { programName = "mutt"
    , programTitle = "mutt"
    , programCommand = "mutt"
    , programType = "email"
    }

spt :: TerminalProgram
spt = TerminalProgram
    { programName = "spt"
    , programTitle = "spt"
    , programCommand = "spt"
    , programType = "music"
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

ranger :: TerminalProgram
ranger = TerminalProgram
    { programName = "ranger"
    , programTitle = "ranger"
    , programCommand = "ranger"
    , programType = "file"
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
    where launchCommand = "zsh -c 'tmux -q has-session -t "
                        ++ programType
                        ++ " && tmux -2 attach-session -t "
                        ++ programType
                        ++ " || "
                        ++ programName
                        ++ "'"
