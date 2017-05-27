#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Start redshift if it is installed
if type "redshift" >/dev/null 2>&1; then
    pgrep redshift &> /dev/null || redshift &> /dev/null &
fi

# Set the keyboard layout to french canadian
setxkbmap -layout ca -variant fr
# Start urxvt daemon
urxvtd -q -f -o
# Spawn a urxvt terminal
urxvtc &

# Create a named pipe for xmobar to be able to read volume levels
if type "control-volume" >/dev/null 2>&1; then
    control-volume --create-pipe
fi

# Create the song indicator pipe and put a default value
if type "create-named-pipe" >/dev/null 2>&1; then
    create-named-pipe "$HOME/.song-information" "Nothing playing"
fi

# Set the background image
if [ -f "${HOME}/.fehbg" ]; then
    sh ~/.fehbg
fi

# Run xbindkeys if it is installed and not running
if type "xbindkeys" >/dev/null 2>&1 && ! pidof -x "xbindkeys" >/dev/null 2>&1; then
    if ! type "xmonad" >/dev/null 2>&1 || [ "$(xmonad --version | awk -F'.' '{print $2}')" -lt 13 ]; then
        xbindkeys
    fi
fi

if [ "$(xrandr | grep -v '^\s\+' | grep -cv '^Screen')" -ge 2 ]; then
    xrandr --output eDP-1 --auto --output HDMI-1 --auto --noprimary
fi

if type "compton" >/dev/null 2>&1; then
    compton --daemon
fi

# Launch XMonad
exec xmonad
