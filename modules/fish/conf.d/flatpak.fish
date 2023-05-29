# Flatpak directories don't get picked up anymore, so we'll mimic /etc/profile.d/flatpak*.sh
set -ga fish_user_paths ~/.local/share/flatpak/exports/bin /var/lib/flatpak/exports/bin
set -gxa --path XDG_DATA_DIRS ~/.local/share/flatpak/exports/share
if command -vq flatpak
    for install_dir in (flatpak --installations)
        set -gxa XDG_DATA_DIRS $install_dir/exports/share
    end
end

# These should also be here
set -gxa XDG_DATA_DIRS /usr/local/share /usr/share /var/lib/snapd/desktop
