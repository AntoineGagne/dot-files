HISTFILE=~/.zsh/histfile
HISTSIZE=50000
SAVEHIST=50000

setopt COMPLETE_ALIASES
setopt autocd
setopt extendedglob
setopt nomatch
setopt notify
setopt promptsubst
bindkey -v
zstyle :compinstall filename '/home/twain/.zshrc'

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

source "${HOME}/.bash/functions/colors"
source "${HOME}/.bash/.bash_aliases"
source "${HOME}/.bash/.bash_functions"

_completemarks() {
    reply=($(ls ${MARKPATH}))
}
compctl -K _completemarks jump
compctl -K _completemarks j
compctl -K _completemarks unmark
compctl -K _completemarks um

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
elif [ -f /usr/bin/virtualenvwrapper.sh ]; then
    source /usr/bin/virtualenvwrapper.sh
fi

source "${HOME}/.prompt"

prompt() {
    local -r _last_command_exit_status="$(last_command_exit_status "${1}")"
    local -r _virtualenv_info="$(virtualenv_info)"
    local -r _current_directory_information="$(current_directory_information)"
    local -r _git_workspace="$(__git_ps1)"

    cat << EOF
┌─${_last_command_exit_status}${_virtualenv_info}──$(machine_information)──[%B%F{blue}%4~%f%b]──${_current_directory_information}${_git_workspace}
└─╼ λ 
EOF
}

PS1='$(prompt "${?}")'
