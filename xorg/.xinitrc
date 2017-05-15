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
if [ -f "${HOME}/.fehbg " ]; then
    sh ~/.fehbg &
fi

# Launch XMonad
exec xmonad
