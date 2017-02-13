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

## Note on ~/.Xresources
The settings contained in `./user/xorg/.Xresources` are used in the `wm-general`
and `fonts` packages.

## WM configuration
I've recently switched from bspwm to i3-gaps. These packages are used in both
environments.

### General packaages
-   compton
-   feh
-   polkit-gnome
-   rofi

#### Panel 
-   dunst
-   network-manager-applet
-   redshift
-   pavucontrol
-   polkit-gnome
-   polybar
-   volumeicon

#### Theming
-   Arc GTK theme
-   ttf-google-fonts (Cantarell, Open Sans and Roboto)
-   Numix Square icons

#### Optional utilities used in the keymapping
-   emacs
-   imgurbash2
-   maim
-   slop
-   xclip
-   xdotool

### bspwm
-   bspvm
-   sxhkd

### i3
-   i3-gaps
