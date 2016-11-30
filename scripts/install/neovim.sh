#!/usr/bin/env bash

if ! type -P nvim &>/dev/null; then
  echo "neovim mist";
  exit 1;
fi

pacman -Qi python-neovim &>/dev/null
if [ $? -ne 0 ]; then
  echo "De python-neovim package mist";
  exit 1;
fi

pacman -Qi neovim-drop-in &>/dev/null
if [ $? -ne 0 ]; then
  echo "De neovim-drop-in package mist";
  exit 1;
fi

if [ ! -d $HOME/.config/nvim/ ]; then
  echo "De neovimconfiguratie is nog niet gesymlinked"
  exit 1
fi

# Installeer alle neovim plugins
echo "========================================"
echo "Neovim plugins installeren..."
echo "========================================"

nvim +PlugInstall +PlugUpdate +PlugClean! +qall

