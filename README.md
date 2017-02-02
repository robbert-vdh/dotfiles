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
The settings contained in `./user/xorg/.Xresources` are used in the `bspwm` and
`fonts` packages.

## bspwm
These packages are used in my bspwm configuration.

-   bspvm
-   compton
-   feh
-   rofi
-   sxhkd

### Panel 
-   dunst (there's a patched version with SVG support in the AUR)
-   network-manager-applet
-   pavucontrol
-   polkit-gnome
-   polybar
-   volumeicon

### Theming
-   Numix Square icons
-   Arc GTK theme

### Optional utilities used in the keymapping
-   imgurbash2
-   maim
-   slop
-   xclip
-   xdotool
