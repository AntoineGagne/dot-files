export PROFILING_MODE=0
if [ "${PROFILING_MODE}" -ne 0 ]; then
    zmodload zsh/zprof
fi

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

lazy_load_nvm() {
    unset -f node nvm npm
    export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
    if [[ -s "$NVM_DIR/nvm.sh" ]]; then
        source "$NVM_DIR/nvm.sh"
    fi
}

node() {
    lazy_load_nvm
    node $@
}

npm() {
    lazy_load_nvm
    npm $@
}

is_command() {
    local -r _command="${1}"
    command -v "${_command}" &>/dev/null
}


fpath=("${HOME}/.zsh/functions/" "${HOME}/.zsh/completions/" $fpath)
autoload -U colors && colors
autoload -U ~/.zsh/functions/*(:t)
autoload -U ~/.zsh/completions/*(:t)

autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
autoload -Uz select-quoted
autoload -Uz select-bracketed
autoload -Uz surround
autoload -Uz select-word-match

autoload -Uz zrecompile
autoload -Uz vcs_info

# On slow systems, checking the cached .zcompdump file to see if it must be 
# regenerated adds a noticable delay to zsh startup.  This little hack restricts 
# it to once a day.  It should be pasted into your own completion file.
#
# The globbing is a little complicated here:
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+24' matches files (or directories or whatever) that are older than 24 hours.
autoload -Uz compinit
if [[ -n ${ZDOTDIR}/.zcompdump(#qN.mh+24) ]]; then
    compinit;
else
    compinit -C;
fi

autoload -Uz promptinit
promptinit

autoload -Uz run-help
# unalias run-help
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

# Delete everything from the cursor to the end of the line
bindkey '^K' kill-line

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

zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name
zstyle ':completion:*:parameters'  list-colors '=*=32'
zstyle ':completion:*:commands' list-colors '=*=1;31'
zstyle ':completion:*:builtins' list-colors '=*=1;38;5;142'
zstyle ':completion:*:aliases' list-colors '=*=2;38;5;128'
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'
zstyle ':completion:*:options' list-colors '=^(-- *)=34'

compdef _precommand rb

if [ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
elif [ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
    export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
fi

if [ -f /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
    export ZSH_AUTOSUGGEST_USE_ASYNC=1
elif [ -f ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
    source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
    export ZSH_AUTOSUGGEST_USE_ASYNC=1
fi

if [ -f "/usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh" ]; then
    source "/usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh"
elif [ -f "/usr/share/virtualenvwrapper/virtualenvwrapper.sh" ]; then
    source "/usr/share/virtualenvwrapper/virtualenvwrapper.sh"
fi

source "${HOME}/.prompt"

zstyle ':vcs_info:*' enable git cvs svn hg
zstyle ':vcs_info:*' actionformats ' ( %b|%a|%u|%c)'
zstyle ':vcs_info:*' formats ' ( %b)'
precmd () { vcs_info }

prompt() {
    local -r _last_command_exit_status="$(last_command_exit_status "${1}")"
    local -r _virtualenv_info="$(virtualenv_info)"
    local -r _kerl_info="$(kerl_info)"
    local -r _current_directory_information="$(current_directory_information)"
    local -r _git_workspace="${vcs_info_msg_0_}"

    cat << EOF
┌─${_last_command_exit_status}${_virtualenv_info}──$(machine_information)──[%B%F{blue}%4~%f%b]──${_current_directory_information}${_kerl_info}${_git_workspace}
└─╼ λ 
EOF
}

PS1='$(prompt "${?}")'
PS2="├╼ "
RPS1='[%F{cyan}%j jobs%f]──[%F{green}%W%f]'

if [[ -f ~/.fzf.zsh ]]; then
    source ~/.fzf.zsh
fi

if is_command tv >/dev/null; then
    eval "$(tv init zsh)"
fi

if type "direnv" >/dev/null 2>&1; then
    eval "$(direnv hook zsh)"
fi

if [[ -f "${HOME}/.ghcup/env" ]]; then
    source "${HOME}/.ghcup/env"
fi

if [[ -d "${HOME}/.kube" ]]; then
    KUBECONFIG_YAMLS="$(ls -1 ~/.kube/*.yaml | xargs)"
    export KUBECONFIG="${KUBECONFIG_YAMLS// /:}"

fi

if is_command bartib >/dev/null; then
    export BARTIB_FILE="${HOME}/.local/state/bartib/activities.log"
    mkdir -p "$(dirname "${BARTIB_FILE}")"
fi

if [[ -d "${HOME}/.cargo/bin" ]]; then
    PATH="${PATH}:${HOME}/.cargo/bin"
fi

if [[ -d "${HOME}/.npm-packages" ]]; then
    export NPM_PACKAGES="$HOME/.npm-packages"
    PATH="${PATH}:${NPM_PACKAGES}/bin"
fi

if is_command "dotnet" &>/dev/null; then
    export PATH="${PATH}:/usr/share/dotnet"
    export PATH="${PATH}:${HOME}/.dotnet/tools"
    export DOTNET_ROOT="/usr/share/dotnet"
fi

# Remove duplicate lines in $PATH
export PATH="$(echo "$PATH" | awk -F':' '{for (i=1;i<=NF;++i) print($i)}' | awk '!x[$0]++' | awk '{printf("%s:", $0);}' | sed 's/.$//')"

eval $(keychain --agents ssh,gpg --eval --nogui --quiet id_rsa)


if [ "${PROFILING_MODE}" -ne 0 ]; then
    zprof
fi
