{ config, lib, pkgs, ... }:

{
  home.activation = {
    # Bit of a hacky way to get these config files in the right place. This
    # requires sudo, but it doesn't do anything if the files in /etc are already
    # symlinks to the correct files.
    symlinkPacmanConfigs = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # We'll print a message the first time we require sudo here
      have_symlinked=0

      symlink_if_needed() {
        local filename target_path

        filename="$(basename "$1")"
        target_path="/etc/$filename"
        if ! diff --brief "$target_path" "$1" >/dev/null 2>&1; then
          if [[ $have_symlinked -eq 0 ]]; then
            echo "Symlinking pacman config files to /etc..."
            have_symlinked=1
          fi

          # This needs to use /usr/bin/sudo directly
          $DRY_RUN_CMD /usr/bin/sudo ln -sf $VERBOSE_ARG "$1" "$target_path"
        fi
      }

      symlink_if_needed "${builtins.toPath etc/makepkg.conf}"
      symlink_if_needed "${builtins.toPath etc/pacman.conf}"
      symlink_if_needed "${builtins.toPath etc/paru.conf}"
    '';
  };
}
