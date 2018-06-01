# Dotfiles
These dotfiles are managed using
[GNU Stow](https://www.gnu.org/software/stow/stow.html).

## Installation
Clone and install:

```shell
$ git clone https://github.com/robbert-vdh/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ ./install.sh
```

## Emacs configuration
My Emacs configuration is based on [Doom
Emacs](https://github.com/hlissner/doom-emacs). The files in
`usr/emacs/.emacs.d` can simply be copied or symlinked to wherever Doom is
installed.

## xfce + i3
KDE handles most non-WM shortcuts and things like text rendering and themes.

-   Arc GTK theme
-   arc-kde
-   compton
-   feh
-   i3-gaps-next-git
-   konsole
-   kvantum
-   Liberation Mono font with powerline patch (i.e. ttf-literation-mono-powerline)
-   light-locker
-   qt5ct
-   Papirus icon theme
-   playerctl for Spotify
-   Roboto font
-   xdotool
-   xfce4
-   xfce4-notifyd (should be started as a systemd user service)
-   xfce4-i3-workspaces-plugin-git

### Screenshots
-   imgurbash2
-   maim
-   slop

### Autostarted
-   albert
-   kdeconnect
-   keepassxc
-   network-manager-applet
-   redshift
