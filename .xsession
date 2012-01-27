xsetroot -cursor_name left_ptr

# map caps-lock to escape for faster vim usage
xmodmap -e "clear lock"
xmodmap -e "keycode 0x42 = Escape"

gnome-screensaver &

gnome-settings-daemon &

if [ -x /usr/bin/gnome-power-manager ] ; then
   sleep 1
   gnome-power-manager &
fi

setxkbmap us altgr-intl

. ~/.xsession.local

~/.cabal/bin/xmonad
