set -gx EDITOR vim
set -gx VISUAL $EDITOR
set -gx PAGER less
set -gx BROWSER firefox

set -gx RUST_SRC_PATH "$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"

# Merge local directories with $PATH
set -g fish_user_paths ~/.cargo/bin ~/.dotfiles/bin ~/.local/bin

# Enable colours in manpages
set -gx LESS_TERMCAP_mb (set_color --bold red)
set -gx LESS_TERMCAP_md (set_color --bold red)
set -gx LESS_TERMCAP_me (set_color normal)
set -gx LESS_TERMCAP_se (set_color normal)
set -gx LESS_TERMCAP_so (set_color --reverse)
set -gx LESS_TERMCAP_ue (set_color normal)
set -gx LESS_TERMCAP_us (set_color --bold green)
