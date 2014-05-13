#!/bin/bash

export _JAVA_AWT_WM_NONREPARENTING=1

nohup runhaskell ~/bin/session-monad.hs &

# Load monitor calibration early
dispwin -d 1 -L
dispwin -d 2 -L 2> /dev/null

export PATH=~/bin:~/.cabal/bin:$PATH

eval $(ssh-agent)
eval $(cat ~/.fehbg)

setxkbmap -option caps:super \
          -option grp:ctrl_shift_toggle \
          -layout "us-intl, gr" \
          -variant intl

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
workrave &
xfce4-panel &

exec xmonad