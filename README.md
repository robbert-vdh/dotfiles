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

## KDE
-   Arc GTK theme
-   arc-kde
-   kdeconnect
-   konsole
-   kvantum
-   Liberation Mono font
-   Numix Square icons
-   Papirus icon theme
-   Roboto font
-   spectacle

## i3
KDE handles most non-WM shortcuts and things like text rendering and themes.

-   compton
-   feh
-   i3-gaps-next-git
-   wmctrl

### Autostarted
-   keepassxc
-   redshift

Some old configuration is stored in the [backup](backup) directory.
