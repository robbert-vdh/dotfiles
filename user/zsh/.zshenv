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

export RUST_SRC_PATH='/home/robbert/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
