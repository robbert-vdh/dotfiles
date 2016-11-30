#!/usr/bin/env bash

if ! type -P zsh &>/dev/null; then
  echo "zsh mist";
  exit 1;
fi

if ! type -P git &>/dev/null; then
  echo "git mist";
  exit 1;
fi

# Installeer base16 ondersteuning voor zsh
if [ ! -d $HOME/.config/base16-shell/ ]; then
  echo "========================================"
  echo "Base16 Shell installeren..."
  echo "========================================"

  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
fi

# Installeer base16 themes voor gnome-terminal
if [ ! -d $HOME/.config/base16-gnome-terminal/ ]; then
  echo "========================================"
  echo "Base16 gnome-terminal thema installeren..."
  echo "========================================"

  git clone https://github.com/chriskempson/base16-gnome-terminal.git ~/.config/base16-gnome-terminal
  bash $HOME/.config/base16-gnome-terminal/base16-tomorrow.dark.sh

  echo "Herstart de terminal en kies het 'Base16 Tomorrow Night' profiel"
fi

# Installeer prezto
if [ ! -d $HOME/.zprezto/ ]; then
  echo "========================================"
  echo "Prezto installeren..."
  echo "========================================"
  git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
  chsh -s /bin/zsh

  echo "Draai $(dirname $(dirname $(pwd)))/install om de symlinks te maken"
  echo "Start daarna de terminal opnieuw op om naar zsh over te schakelen"
fi

