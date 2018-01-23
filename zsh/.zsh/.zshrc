HISTFILE=~/.zsh/histfile
HISTSIZE=50000
SAVEHIST=50000

# If this is set, zsh sessions will append their history list to the history
# file, rather than replace it. Thus, multiple parallel zsh sessions will all
# have the new entries from their history lists added to the history file, in
# the order that they exit. The file will still be periodically re-written to
# trim it when the number of lines grows 20% beyond the value specified by
# $SAVEHIST (see also the HIST_SAVE_BY_COPY option).
setopt appendhistory

# If a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the `cd` command to that
# directory.
setopt autocd

# Automatically list choices on an ambiguous completion.
setopt autolist

# Prevents aliases on the command line from being internally substituted before
# completion is attempted. The effect is to make the alias a distinct command
# for completion purposes.
setopt completealiases

# Treat the '#', '~' and '^' characters as part of patterns for filename
# generation, etc. (An initial unquoted '~' always produces named directory
# expansion.)
setopt extendedglob

# Note the location of each command the first time it is executed. Subsequent
# invocations of the same command will use the saved location, avoiding a path
# search. If this option is unset, no path hashing is done at all. However,
# when CORRECT is set, commands whose names do not appear in the functions or
# aliases hash tables are hashed in order to avoid reporting them as spelling
# errors.
setopt hashcmds

# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list (even if it is not the
# previous event).
setopt histignorealldups

# Do not enter command lines into the history list if they are duplicates of
# the previous event.
setopt histignoredups

# Remove command lines from the history list when the first character on the
# line is a space, or when one of the expanded aliases contains a leading
# space. Note that the command lingers in the internal history until the next
# command is entered before it vanishes, allowing you to briefly reuse or edit
# the line. If you want to make it vanish right away without entering another
# command, type a space and press return. 
setopt histignorespace

# Remove superfluous blanks from each command line being added to the history
# list.
setopt histreduceblanks

# Allow comments even in interactive shells.
setopt interactivecomments

# This option works when `AUTO_LIST` or `BASH_AUTO_LIST` is also set. If there
# is an unambiguous prefix to insert on the command line, that is done without
# a completion list being displayed; in other words, auto-listing behaviour
# only takes place when nothing would be inserted. In the case of
# `BASH_AUTO_LIST`, this means that the list will be delayed to the third call
# of the function.
setopt listambiguous

# When listing files that are possible completions, show the type of each file
# with a trailing identifying mark. 
setopt listtypes

# On an ambiguous completion, instead of listing possibilities or beeping,
# insert the first match immediately. Then when completion is requested again,
# remove the first match and insert the second match, etc. When there are no
# more matches, go back to the first one again. `reverse`-menu-complete may be
# used to loop through the list in the other direction. This option overrides
# `AUTO_MENU`.
setopt menucomplete

# If a pattern for filename generation has no matches, print an error, instead
# of leaving it unchanged in the argument list. This also applies to file
# expansion of an initial '~' or '='.
setopt nomatch

# Report the status of background jobs immediately, rather than waiting until
# just before printing a prompt.
setopt notify

# If set, parameter expansion, command substitution and arithmetic expansion
# are performed in prompts. Substitutions within prompts do not affect the
# command status.
setopt promptsubst

# This option both imports new commands from the history file, and also causes
# your typed commands to be appended to the history file (the latter is like
# specifying INC_APPEND_HISTORY). The history lines are also output with
# timestamps ala EXTENDED_HISTORY (which makes it easier to find the spot where
# we left off reading the file after it gets re-written).
#
# By default, history movement commands visit the imported lines as well as the
# local lines, but you can toggle this on and off with the set-local-history
# zle binding. It is also possible to create a zle widget that will make some
# commands ignore imported commands, and some include them.
#
# If you find that you want more control over when commands get imported, you
# may wish to turn SHARE_HISTORY off, INC_APPEND_HISTORY on, and then manually
# import commands whenever you need them using 'fc -RI'.
setopt sharehistory

# Remove any right prompt from display when accepting a command line. This may
# be useful with terminals with other cut/paste methods.
setopt transientrprompt

# The time the shell waits, in hundredths of seconds, for another key to be
# pressed when reading bound multi-character sequences.
export KEYTIMEOUT=10

# If nonnegative, commands whose maximum resident set size (roughly speaking,
# main memory usage) in megabytes is greater than this value have timing
# statistics reported. The format used to output statistics is the value of the
# TIMEFMT parameter, which is the same as for the REPORTTIME variable and the
# time builtin; note that by default this does not output memory usage.
# Appending " max RSS %M" to the value of TIMEFMT causes it to output the value
# that triggered the report. If REPORTTIME is also in use, at most a single
# report is printed for both triggers. This feature requires the getrusage()
# system call, commonly supported by modern Unix-like systems.
export REPORTMEMORY=1000

# If nonnegative, commands whose combined user and system execution times
# (measured in seconds) are greater than this value have timing statistics
# printed for them. Output is suppressed for commands executed within the line
# editor, including completion; commands explicitly marked with the time
# keyword still cause the summary to be printed in this case.
export REPORTTIME=1


# If set, used to give the indentation between the right hand side of the right
# prompt in the line editor as given by RPS1 or RPROMPT and the right hand side
# of the screen. If not set, the value 1 is used.
#
# Typically this will be used to set the value to 0 so that the prompt appears
# flush with the right hand side of the screen. This is not the default as many
# terminals do not handle this correctly, in particular when the prompt appears
# at the extreme bottom right of the screen. Recent virtual terminals are more
# likely to handle this case correctly. Some experimentation is necessary.
export ZLE_RPROMPT_INDENT=0

fpath=("${HOME}/.zsh/functions/" $fpath)
autoload -U colors && colors
autoload batch-convert-audio-file
autoload convert-audio-file
autoload convert-svg-to-favicon
autoload display-available-colors
autoload download-playlists
autoload download-video-playlist
autoload download-video-playlists
autoload extract
autoload jump
autoload mark
autoload marks
autoload md-to-html
autoload parallel-download
autoload rb
autoload split-audio-file
autoload start-tmux-sessions
autoload tabs-to-space
autoload top10
autoload unmark
autoload upload-to-link
autoload weather
autoload youtube-dl
autoload rationalise-dot

autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
autoload -Uz select-quoted
autoload -Uz select-bracketed
autoload -Uz surround
autoload -Uz select-word-match

autoload -Uz zrecompile
autoload -Uz vcs_info
autoload predict-on
predict-on

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

autoload -Uz run-help
unalias run-help
alias help=run-help

autoload zkbd

bindkey -v

zle -N select-quoted
zle -N select-bracketed
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
zle -N select-in-directory select-word-match
zle -N select-a-directory select-word-match
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
[[ -n "$key[Up]"   ]] && bindkey -- "$key[Up]"   up-line-or-beginning-search
[[ -n "$key[Down]" ]] && bindkey -- "$key[Down]" down-line-or-beginning-search

_pip_completion() {
    local words cword
    read -Ac words
    read -cn cword
    reply=( $( COMP_WORDS="$words[*]" \
        COMP_CWORD=$(( cword-1 )) \
        PIP_AUTO_COMPLETE=1 $words[1] ) \
    )
}
compctl -K _pip_completion pip


zle-keymap-select () {
    case $KEYMAP in
        vicmd)
            echo -n '\e[1 q'
            ;;
        viins|main)
            echo -n '\e[5 q'
            ;;
    esac
}
echo -n '\e[5 q'

zle -N zle-keymap-select

bindkey -v
zstyle :compinstall filename "${HOME}/.zshrc"

zle -N rationalise-dot
bindkey . rationalise-dot
bindkey -M isearch . self-insert

# See https://dougblack.io/words/zsh-vi-mode.html
# Use vim cli mode
bindkey '^P' up-history
bindkey '^N' down-history

# backspace and ^h working even after
# returning from command mode
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char

# ctrl-w removed word backwards
bindkey '^w' backward-kill-word

# ctrl-r starts searching history backward
bindkey '^r' history-incremental-search-backward

# Enable surround text-objects
bindkey -a cs change-surround
bindkey -a ds delete-surround
bindkey -a ys add-surround
bindkey -M visual S add-surround

# Enable parenthesis text-object
for m in visual viopp; do
    for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
        bindkey -M $m $c select-bracketed
    done
done

for m in visual viopp; do
    for c in {a,i}{\',\",\`}; do
        bindkey -M $m $c select-quoted
    done
done


if [[ -f "${HOME}/.opam/opam-init/init.zsh" ]]; then
    source "${HOME}/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null || true
fi

source "${HOME}/.profile"
source "${HOME}/.aliases/movement"
source "${HOME}/.aliases/date"
source "${HOME}/.aliases/system"
source "${HOME}/.aliases/editing"
source "${HOME}/.aliases/file"
source "${HOME}/.aliases/programs"
source "${HOME}/.aliases/network"

_completemarks() {
    reply=($(ls ${MARKPATH}))
}
compctl -K _completemarks jump
compctl -K _completemarks j
compctl -K _completemarks unmark
compctl -K _completemarks um

zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name

compctl -GRFBm rb

if [ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
elif [ -f /usr/bin/virtualenvwrapper.sh ]; then
    source /usr/bin/virtualenvwrapper.sh
fi

source "${HOME}/.prompt"

zstyle ':vcs_info:*' enable git cvs svn hg
zstyle ':vcs_info:*' actionformats ' ( %b|%a|%u|%c)'
zstyle ':vcs_info:*' formats ' ( %b)'
precmd () { vcs_info }

prompt() {
    local -r _last_command_exit_status="$(last_command_exit_status "${1}")"
    local -r _virtualenv_info="$(virtualenv_info)"
    local -r _current_directory_information="$(current_directory_information)"
    local -r _git_workspace="${vcs_info_msg_0_}"

    cat << EOF
┌─${_last_command_exit_status}${_virtualenv_info}──$(machine_information)──[%B%F{blue}%4~%f%b]──${_current_directory_information}${_git_workspace}
└─╼ λ 
EOF
}

PS1='$(prompt "${?}")'
RPS1='[%F{cyan}%j jobs%f]──[%F{green}%W%f]'
