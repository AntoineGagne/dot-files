HISTFILE=~/.zsh/histfile
HISTSIZE=50000
SAVEHIST=50000

setopt COMPLETE_ALIASES
setopt appendhistory autocd extendedglob nomatch notify
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

if [ -f /usr/share/git/completion/git-prompt.sh ]; then
    source /usr/share/git/completion/git-prompt.sh
fi

if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    source /usr/local/bin/virtualenvwrapper.sh
elif [ -f /usr/bin/virtualenvwrapper.sh ]; then
    source /usr/bin/virtualenvwrapper.sh
fi

virtualenv_info() {
    if [[ -n "$VIRTUAL_ENV" ]]; then
        # Strip out the path and just leave the environment's name
        venv="$(basename ${VIRTUAL_ENV})"
    else
        venv=''
    fi
    [[ -n "$venv" ]] && echo "──[$venv]"
}

last_command_exit_status() {
    local -ri _exit_code="${1}"
    local -i _code_color=${GREEN}
    local _code_symbol="✔"
    if [ "${_exit_code}" -ne 0 ]; then
        _code_color=1
        _code_symbol="✘"
    fi
    echo -n "[$(with_bold with_color ${_code_color} echo -n "${_code_symbol}")]"
}

current_directory_information() {
    local -r _current_directory_information="$(ls -lah \
        | awk '/total/ {
            total_space=$2
          }
          END {
            print NR " files, " total_space
          }'
    )"
    echo -n "[${_current_directory_information}]"
}

PS1='┌─$(last_command_exit_status "${?}")'
PS1=$PS1'$(virtualenv_info)'
PS1=$PS1'──[%B%F{green}%n@%m%f%b]'
PS1=$PS1'──[%B%F{blue}%4~%f%b]'
PS1=$PS1'──$(current_directory_information)'
PS1=$PS1'$(__git_ps1)'
PS1=$PS1$'\n''└─╼ λ '
