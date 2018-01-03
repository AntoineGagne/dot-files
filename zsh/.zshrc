HISTFILE=~/.zsh/histfile
HISTSIZE=50000
SAVEHIST=50000

setopt COMPLETE_ALIASES
setopt appendhistory autocd extendedglob nomatch notify
bindkey -v
zstyle :compinstall filename '/home/twain/.zshrc'

autoload -Uz compinit
compinit

autoload -Uz promptinit
promptinit

source ~/.bash/.bash_aliases
source ~/.bash/.bash_functions

PS1='┌─[]──[%B%F{green}%n@%m%f%b]──[%B%F{blue}%4~%f%b]──[]'
PS1=$PS1$'\n''└─╼ λ '
