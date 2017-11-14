typeset -gU cdpath fpath mailpath path

path=(
  /usr/local/{bin,sbin}
  $HOME/.cargo/bin
  $HOME/.dotfiles/bin
  $HOME/.local/bin
  $path
)

export EDITOR='nvim'
export VISUAL=$EDITOR
export PAGER='less'
export BROWSER='firefox-developer'
