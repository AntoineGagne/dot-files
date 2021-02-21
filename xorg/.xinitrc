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
    for _file in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "${_file}" ] && . "${_file}"
    done
    unset _file
fi

# Set the keyboard layout to french canadian
setxkbmap -layout ca -variant fr
alacritty &

# Set the background image
if [ -f "${HOME}/.fehbg" ]; then
    sh ~/.fehbg
fi

# if type "corsair-keyboard" >/dev/null 2>&1; then
#     corsair-keyboard
# fi

if type "compton" >/dev/null 2>&1; then
    compton -b
fi

# Remove annoying bell sound
if type "xset" >/dev/null 2>&1; then
    xset -b
fi

if type "xscreensaver" >/dev/null 2>&1; then
    xscreensaver -nosplash &
fi

systemctl --user import-environment DISPLAY

# Launch XMonad
exec dbus-launch --autolaunch "$(dbus-uuidgen --get)" xmonad
