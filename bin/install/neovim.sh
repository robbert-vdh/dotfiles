#!/usr/bin/env bash

if ! type -P nvim &>/dev/null; then
  echo "neovim is missing";
  exit 1;
fi

if ! type -P git &>/dev/null; then
  echo "git is missing";
  exit 1;
fi

pacman -Qi python-neovim &>/dev/null
if [ $? -ne 0 ]; then
  echo "The python-neovim is missing";
  exit 1;
fi

pacman -Qi neovim-plug &>/dev/null
if [ $? -ne 0 ]; then
  echo "The neovim-plug package is missing";
  exit 1;
fi

pacman -Qi neovim-drop-in &>/dev/null
if [ $? -ne 0 ]; then
  echo "The neovim-drop-in package is missing";
  exit 1;
fi

if [ ! -d $HOME/.config/nvim/ ]; then
  echo "The Neovim configuration has not been installed yet"
  exit 1
fi

echo "========================================"
echo "Installing Neovim plugins..."
echo "========================================"

nvim +PlugInstall +PlugUpdate +PlugClean! +qall
