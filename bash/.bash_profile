#
# ~/.bash_profile
#

if [[ -f ~/.profile ]]; then 
    source ~/.profile
fi

if [[ -f ~/.bashrc ]]; then
    . ~/.bashrc
fi

export PATH="$HOME/.cargo/bin:$PATH"
