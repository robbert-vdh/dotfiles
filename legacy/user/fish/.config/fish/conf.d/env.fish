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

# This is supposed to be set automatically. It isn't.
set -gx DEBUGINFOD_URLS 'https://debuginfod.archlinux.org'

# Always enable the fsync patches in Wine
set -gx WINEFSYNC 1
# Workaround for https://bugs.kde.org/show_bug.cgi?id=414785
# https://www.reddit.com/r/kde/comments/h0u6j7/choppy_nvidia_x11_performance_on_plasma_519_with/ftoa0jv/
# Doesn't actually fully resolve the issue, but it at least seems a tiny bit
# better
set -gx __GL_YIELD usleep

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

# Not really used anymore, but this was once useful in conjunction with GNU
# Global to get tags in .scss files
set -gx GTAGSLABEL pygments

# Enable colours in manpages
set -gx LESS_TERMCAP_mb (set_color --bold red)
set -gx LESS_TERMCAP_md (set_color --bold red)
set -gx LESS_TERMCAP_me (set_color normal)
set -gx LESS_TERMCAP_se (set_color normal)
set -gx LESS_TERMCAP_so (set_color --reverse)
set -gx LESS_TERMCAP_ue (set_color normal)
set -gx LESS_TERMCAP_us (set_color --bold green)