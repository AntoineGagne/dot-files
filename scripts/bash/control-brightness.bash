#!/bin/bash

# Mostly taken from: https://bbs.archlinux.org/viewtopic.php?id=134972 (Wed May 24 20:21:45 EDT 2017)

# base directory for backlight class
basedir="/sys/class/backlight"

# get the backlight handler
handler="$basedir/$(ls $basedir)"

# get current brightness
current_brightness=$(cat "$handler/brightness")

# get max brightness
max_brightness=$(cat "$handler/max_brightness")

# get current brightness %
current_brightness_percentage=$(( 100 * current_brightness / max_brightness ))

# calculate new brightness %
new_brightness_percentage=$((current_brightness_percentage + $1))

# calculate new brightness value
new_brightness=$(( max_brightness * new_brightness_percentage / 100 ))

if [ $new_brightness -le "$max_brightness" ] && [ $new_brightness -ge 0 ]; then
    # set the new brightness value
    echo "$new_brightness" > "$handler/brightness"
elif [ $new_brightness -lt 0 ]; then
    # If it is lower than the minimum brightness, set its value to 0%
    echo "0" > "$handler/brightness"
else
    # If it is higher than the maximum brightness, set its value to 100%
    echo "$max_brightness" > "$handler/brightness"
fi
