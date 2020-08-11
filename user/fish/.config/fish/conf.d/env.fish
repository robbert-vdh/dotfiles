set -gx EDITOR nvim
set -gx VISUAL $EDITOR
set -gx PAGER less
set -gx BROWSER firefox
set -gx LESS "--RAW-CONTROL-CHARS --ignore-case --jump-target=4"

# Let Firefox (and other compatible applications) use KDE dialogs
set -gx GTK_USE_PORTAL 1
# Always build with 8 threads by default
set -gx MAKEFLAGS "-j8"
# Needed for some tooling
set -gx RUST_SRC_PATH "$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
# Always enable the fsync patches in Wine
set -gx WINEFSYNC 1

# Merge local directories with $PATH
set -g fish_user_paths ~/.cabal/bin ~/.cargo/bin ~/.dotfiles/bin ~/.ghcup/bin ~/.local/bin

# Enable colours in manpages
set -gx LESS_TERMCAP_mb (set_color --bold red)
set -gx LESS_TERMCAP_md (set_color --bold red)
set -gx LESS_TERMCAP_me (set_color normal)
set -gx LESS_TERMCAP_se (set_color normal)
set -gx LESS_TERMCAP_so (set_color --reverse)
set -gx LESS_TERMCAP_ue (set_color normal)
set -gx LESS_TERMCAP_us (set_color --bold green)
