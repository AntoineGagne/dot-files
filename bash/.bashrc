# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
# We use preexec and precmd hook functions for Bash
# If you have anything that's using the Debug Trap or PROMPT_COMMAND 
# change it to use preexec or precmd
# See also https://github.com/rcaloras/bash-preexec

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=20000
HISTFILESIZE=20000

# After each command, append to the history file and reread it
# Taken from: https://unix.stackexchange.com/questions/1288/preserve-bash-history-in-multiple-terminal-windows (Fri Jun 23 11:13:43 EDT 2017)
export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# Cycling autocomplete when pressing tab
# bind TAB:menu-complete

# Append to the history file, don't overwrite it
shopt -s histappend

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# If set, a command name that is the name of a directory is executed
# as if it were the argument to the `cd` command. This options is
# only used by interactive shells.
shopt -s autocd

# If set, minor errors in the spelling of a directory components in
# a `cd` command will be corrected. The errors checked for are
# transposed characters, a missing character, and a character too
# many. If a correction is found, the corrected path is printed, and
# the command proceeds. This option is only used by interactive
# shells.
shopt -s cdspell

# If set, Bash attempts spelling correction on directory names during
# word completion if the directory name initially supplied does not
# exist.
shopt -s dirspell

# If set, bash attempts to save all lines of a multiple-line command in the 
# same history entry.  This allows easy re-editing of multi-line commands.
shopt -s cmdhist

# If set, bash includes filenames beginning with a `.' in the results of 
# pathname expansion.
shopt -s dotglob

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

if [ -f "$(which virtualenvwrapper_lazy.sh)" ]; then
    source "$(which virtualenvwrapper_lazy.sh)"
elif [ -f "$(which virtualenvwrapper.sh)" ]; then
    source "$(which virtualenvwrapper.sh)"
fi

if type -f "pandoc" >/dev/null 2>&1; then
    eval "$(pandoc --bash-completion)"
fi

# Set Stack autocompletion on tab
if type -f "stack" >/dev/null 2>&1; then
    eval "$(stack --bash-completion-script stack)"
fi

# Add autocompletion to the custom Haskell scripts
if type -f "create-gitignore" > /dev/null 2>&1; then
    eval "$(create-gitignore --bash-completion-script create-gitignore)"
fi

if type -f "pinfo" > /dev/null 2>&1; then
    alias info='pinfo'
fi

if type -f "beet" > /dev/null 2>&1; then
    source ~/.bash/completions/beet-completion.bash
fi

source ~/.bash/completions/functions-completions.bash

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash/.bash_aliases ]; then
    . ~/.bash/.bash_aliases
fi

if [ -f ~/.bash/.bash_functions ]; then
    . ~/.bash/.bash_functions
fi

# Autocomplete marks
complete -F _completemarks jump unmark
complete -F _completemarks j um

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    source /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    source /etc/bash_completion
  fi
fi

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    source /usr/share/git/completion/git-prompt.sh
fi

source "${HOME}/.prompt"

current_working_directory() {
    local -r _maximum_path_length="$(("$(tput cols)" / 3))"
    local -r _abbreviated_path_symbol='[…]'
    local _current_path="${PWD/${HOME}/'~'}"
    local -ri _current_path_length="$(echo -n "${_current_path}" | wc -c | tr -d " ")"

    if [[ "${_current_path_length}" -gt "${_maximum_path_length}" ]]; then
        _current_path="$(echo -n "${_current_path}" \
            | awk -F'/' -v abbreviated_path_symbol="${_abbreviated_path_symbol}" '{
                print $1 "/" $2 "/" abbreviated_path_symbol "/" $(NF - 1) "/" $(NF)
              }'
        )"
    fi

    echo -n "[$(with_bold with_color 4 echo -n "${_current_path}")]"
}

prompt() {
    local -r _last_command_exit_status="$(last_command_exit_status "${1}")"
    local -r _virtualenv_info="$(virtualenv_info)"
    local -r _current_working_directory="$(current_working_directory)"
    local -r _current_directory_information="$(current_directory_information)"
    local -r _git_workspace="$(__git_ps1)"

    cat << EOF
┌─${_last_command_exit_status}${_virtualenv_info}──$(machine_information)──${_current_working_directory}──${_current_directory_information}${_git_workspace}
└─╼ λ 
EOF
}
PS1='$(prompt "${?}")'

[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# added by travis gem
[ -f /home/angagne/.travis/travis.sh ] && source /home/angagne/.travis/travis.sh
