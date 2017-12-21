typeset -gU cdpath fpath mailpath path

path=(
  /usr/local/{bin,sbin}
  $HOME/.cargo/bin
  $HOME/.dotfiles/bin
  $HOME/.local/bin
  $path
)

export EDITOR='nvim'
export VISUAL="$EDITOR"
export PAGER='less'
export BROWSER='firefox-developer-edition'

export RUST_SRC_PATH="$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
# Needed until rustfmt-nightly gets their stuff together
#export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib
export LD_LIBRARY_PATH="$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib"
