#!/bin/sh

xhost +SI:localuser:$USER
wmname LG3D

# Set fallback cursor
xsetroot -cursor_name left_ptr

# If Emacs is started in server mode, `emacsclient` is a convenient way to edit
# files in place (used by e.g. `git commit`)
export _JAVA_AWT_WM_NONREPARENTING=1
export VISUAL=emacsclient
export EDITOR="$VISUAL"

exec emacs --with-exwm
