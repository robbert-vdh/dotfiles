#
# User configuration sourced by interactive shells
#

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

# Set the path
typeset -gU cdpath fpath mailpath path

path=(
  /usr/local/{bin,sbin}
  $HOME/.cargo/bin
  $HOME/.dotfiles/bin
  $HOME/.local/bin
  $path
)

# Configure editors
export EDITOR="nvim"
export VISUAL="nvim"
export PAGER="less"
export BROWSER="firefox-beta"

# Emacs should use a client process when possible
alias emacs="emacsclient -nca ''"
alias emacsdpi="XDG_CONFIG_HOME=.emacs.d/xdg_config_home/ GDK_SCALE=1 GDK_DPI_SCALE=1 emacs"

# Use base16 colors in VIM
BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# Fix opening new windows in Gnome SHell
source /etc/profile.d/vte.sh
