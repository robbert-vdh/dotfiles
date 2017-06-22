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

## Spacemacs layers
I've got Spacemacs layers for the following packages:

-   irony-mode (faster C/C++ auto completion)
-   languagetool (grammar correction)
-   platformio (embedded system development)

They can be found over
at [user/emacs/.emacs.d/private/layers](user/emacs/.emacs.d/private/layers),
along with installation instructions.

## Note on ~/.Xresources
The settings contained in `./user/xorg/.Xresources` are used in the `wm-general`
and `fonts` packages.

## WM configuration
-   compton
-   feh
-   i3-gaps
-   lxqt-policykit
-   rofi

### Lockscreen
-   i3lock
-   i3lock-fancy-dualmonitors-git
-   xautolock

### Panel 
-   dunst-git
-   network-manager-applet
-   redshift
-   pavucontrol
-   llxqt-policykit agent
-   polybar
-   volumeicon

### Theming
-   Arc GTK theme
-   Liberation Mono font
-   Noto font
-   Numix Square icons
-   Roboto font
-   qt5ct

### Optional utilities used in the keymapping
-   emacs
-   imgurbash2
-   light
-   maim
-   slop
-   xclip
-   xdotool
