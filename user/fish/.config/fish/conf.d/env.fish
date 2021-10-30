set -gx EDITOR nvim
set -gx VISUAL $EDITOR
set -gx PAGER less
set -gx BROWSER firefox
set -gx LESS "--RAW-CONTROL-CHARS --ignore-case --jump-target=4"

# Let Firefox (and other compatible applications) use KDE dialogs
set -gx GTK_USE_PORTAL 1
# Always build with `nproc` threads by default
set -gx MAKEFLAGS -j(nproc)
# Needed for some tooling
set -gx RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/library
# Always enable the fsync patches in Wine
set -gx WINEFSYNC 1

# Merge local directories with $PATH
set -g fish_user_paths ~/.cabal/bin ~/.cargo/bin ~/.dotfiles/bin ~/.ghcup/bin ~/.local/bin

# Flatpak directories don't get picked up anymore, so we'll mimic /etc/profile.d/flatpak*.sh
set -ga fish_user_paths ~/.local/share/flatpak/exports/bin /var/lib/flatpak/exports/bin
set -gx --path XDG_DATA_DIRS ~/.local/share/flatpak/exports/share
if command -vq flatpak
    for install_dir in (flatpak --installations)
        set -gxa XDG_DATA_DIRS $install_dir/exports/share
    end
end

# Neither do the binaries from the CUDA package. These need to be appended to
# the path since CUDA includes an old version of GCC.
set -gx PATH "$PATH:/opt/cuda/bin"

# These should also be here
set -gxa XDG_DATA_DIRS /usr/local/share /usr/share /var/lib/snapd/desktop

# Enable colours in manpages
set -gx LESS_TERMCAP_mb (set_color --bold red)
set -gx LESS_TERMCAP_md (set_color --bold red)
set -gx LESS_TERMCAP_me (set_color normal)
set -gx LESS_TERMCAP_se (set_color normal)
set -gx LESS_TERMCAP_so (set_color --reverse)
set -gx LESS_TERMCAP_ue (set_color normal)
set -gx LESS_TERMCAP_us (set_color --bold green)
