# Merge local directories with $PATH
set -g fish_user_paths ~/.cargo/bin ~/.dotfiles/bin ~/.local/bin

set -g EDITOR nvim
set -g VISUAL $EDITOR
set -g PAGER less
set -g BROWSER firefox-developer-edition

set -g RUST_SRC_PATH "$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
