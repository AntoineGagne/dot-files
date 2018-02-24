# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export MARKPATH=$HOME/.marks

# virtualenvwrapper needed configuration
if [ -d "${HOME}/.virtualenvs" ]; then
    mkdir -p "${HOME}/.virtualenvs"
    export WORKON_HOME=$HOME/.virtualenvs
fi

# Colorful manpages
export LESS='-R '
# enter blinking mode – red
export LESS_TERMCAP_mb=$(printf '\e[01;31m')
# enter double-bright mode – bold, magenta
export LESS_TERMCAP_md=$(printf '\e[01;35m')
# turn off all appearance modes (mb, md, so, us)
export LESS_TERMCAP_me=$(printf '\e[0m')
# leave standout mode
export LESS_TERMCAP_se=$(printf '\e[0m')
# enter standout mode – yellow
export LESS_TERMCAP_so=$(printf '\e[01;33m')
# leave underline mode
export LESS_TERMCAP_ue=$(printf '\e[0m')
# enter underline mode – cyan
export LESS_TERMCAP_us=$(printf '\e[04;36m')

# Added by the Heroku Toolbelt
if [ -d "/usr/local/heroku" ]; then
    export PATH="/usr/local/heroku/bin:$PATH"
fi

# Added by travis gem
if [ -f "${HOME}/.travis/travis.sh" ]; then
    source "${HOME}/.travis/travis.sh"
fi

if [ -f "${HOME}/.pythonrc.py" ]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

if [ -d "${HOME}/.local/bin" ]; then
    export PATH=$PATH:"${HOME}/.local/bin"
fi

if [ -d "${HOME}/.node_modules/bin" ]; then
    export PATH=$PATH:"${HOME}/.node_modules/bin"
    export npm_config_prefix="${HOME}/.node_modules"
fi

if type -f "nvim" >/dev/null 2>&1; then
    export EDITOR=nvim
fi

if type -f "mutt" >/dev/null 2>&1; then
    export COLORFGBG="default;default"
fi

export PATH=$PATH:"${HOME}/.cargo/bin"
export TERM=tmux-256color
# Colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
export QT_STYLE_OVERRIDE=kvantum
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=lcd -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel -Dswing.aatext=true -Dswing.plaf.metal.controlFont=\"DejaVu Sans-14\""
