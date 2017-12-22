{-# LANGUAGE RecordWildCards #-}

module XMonad.Programs.Terminals
    ( TerminalEmulator (..)
    , kitty
    , urxvt
    , launchApp
    , mutt
    , ncmpcpp
    , newsboat
    , weechat
    ) where

data TerminalEmulator = TerminalEmulator
    { terminalName :: String
    , terminalDaemonName :: String
    , terminalTitleOption :: String
    , terminalExecutionOption :: String
    }

instance Show TerminalEmulator where
    show TerminalEmulator { terminalDaemonName = terminalDaemonName' } = terminalDaemonName'

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

data Program = Program
    { programName :: String
    , programTitle :: String
    , programCommand :: String
    , programType :: String
    }

launchApp :: TerminalEmulator -> Program -> String
launchApp TerminalEmulator {..} Program {..} = terminalDaemonName
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

mutt :: Program
mutt = Program
    { programName = "mutt"
    , programTitle = "mutt"
    , programCommand = "mutt"
    , programType = "email"
    }

ncmpcpp :: Program
ncmpcpp = Program
    { programName = "ncmpcpp"
    , programTitle = "ncmpcpp"
    , programCommand = "ncmpcpp"
    , programType = "music"
    }

newsboat :: Program
newsboat = Program
    { programName = "newsboat"
    , programTitle = "newsboat"
    , programCommand = "newsboat"
    , programType = "news"
    }

weechat :: Program
weechat = Program
    { programName = "weechat"
    , programTitle = "weechat"
    , programCommand = "weechat"
    , programType = "chat"
    }
