# Always build with `nproc` threads by default
set -gx MAKEFLAGS -j(nproc)

# This is supposed to be set automatically. It isn't.
if test -z $DEBUGINFOD_URLS
    set -gx DEBUGINFOD_URLS 'https://debuginfod.archlinux.org'
end

# Merge local directories with $PATH
set -g fish_user_paths ~/.cabal/bin ~/.dotfiles/bin ~/.ghcup/bin ~/.local/bin ~/Documenten/projecten/yabridge/build

# Neither do the binaries from the CUDA package. These need to be appended to
# the path since CUDA includes an old version of GCC.
set -gx PATH "$PATH:$HOME/.cargo/bin:/opt/cuda/bin"

# Enable colours in manpages
set -gx LESS_TERMCAP_mb (set_color --bold red)
set -gx LESS_TERMCAP_md (set_color --bold red)
set -gx LESS_TERMCAP_me (set_color normal)
set -gx LESS_TERMCAP_se (set_color normal)
set -gx LESS_TERMCAP_so (set_color --reverse)
set -gx LESS_TERMCAP_ue (set_color normal)
set -gx LESS_TERMCAP_us (set_color --bold green)
