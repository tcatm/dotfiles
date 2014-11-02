#!/bin/bash

lidmon() {
  dbus-monitor --system 'path=/org/freedesktop/UPower' | grep --line-buffered LidIsClosed | while read a; do scr; done
}

export _JAVA_AWT_WM_NONREPARENTING=1

# Load monitor calibration early
dispwin -d 1 -L
dispwin -d 2 -L 2> /dev/null

export PATH=~/bin:~/.cabal/bin:$PATH

eval $(ssh-agent)
eval $(cat ~/.fehbg)

setxkbmap -option caps:super \
					-option grp:ctrl_shift_toggle \
					-option nodeadkeys \
					-layout us \
					-variant altgr-intl

xset s default
xset dpms 600 600 600
xset b off

xsetroot -cursor_name left_ptr
xrdb ~/.Xdefaults

synclient TouchpadOff=1

unclutter -noevents & 
nm-applet &
keepassx&
cryptkeeper &
seafile-applet &
bin/pulseosd &
taffybar &
synapse -s &
lidmon &
xss-lock -- i3lock -ti ~/bin/lockbg.png &

exec xmonad
