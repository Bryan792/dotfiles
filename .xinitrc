xrdb ~/.Xresources 
xset +fp /usr/share/fonts/local 
xset +fp /usr/share/fonts/artwiz-fonts 
xset fp rehash 
#xcalib /home/sunn/x230.icm &
xsetroot -cursor_name left_ptr &
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape' &
#hsetroot -fill /home/sunn/Pictures/spring3_DESKTOP.jpg
unclutter -grab -noevents -root &
compton -cbC
xterm &
vmware-user-suid-wrapper
dbus-launch xmonad --replace
