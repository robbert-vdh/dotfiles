#!/usr/bin/env fish

if status --is-login && test -n "$DESKTOP_SESSION"
    set -x (gnome-keyring-daemon --start --components=ssh,secrets | string split "=")
end
