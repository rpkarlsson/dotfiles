#!/usr/bin/env sh
# The Sway configuration file in ~/.config/sway/config calls this script.
# You should see changes to the status bar after saving this script.
# If not, do "killall swaybar" and $mod+Shift+c to reload the configuration.
date_formatted=$(date "+%a %F %H:%M")

battery_status=$(cat /sys/class/power_supply/BAT0/capacity)

# "amixer -M" gets the mapped volume for evaluating the percentage which
# is more natural to the human ear according to "man amixer".
# Column number 4 contains the current volume percentage in brackets, e.g.,
# "[36%]". Column number 6 is "[off]" or "[on]" depending on whether sound
# is muted or not.
# "tr -d []" removes brackets around the volume.
# Adapted from https://bbs.archlinux.org/viewtopic.php?id=89648
audio_volume=$(amixer -M get Master |\
awk '/Front Right.+/ {print $6=="[off]" ? "---":  $5}' |\
tr -d [])

echo "s: "$audio_volume "b: "$battery_status% $date_formatted
