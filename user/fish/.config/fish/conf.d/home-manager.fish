#!/usr/bin/env fish

# This relies on babelfish, and it should only be done from the login shell
if status --is-login && test -n "$DESKTOP_SESSION"
    set home_manager_env_Vars ~/.nix-profile/etc/profile.d/hm-session-vars.sh
    if test -f $home_manager_env_Vars && command -vq babelfish
        babelfish <$home_manager_env_Vars | source
    end
end
