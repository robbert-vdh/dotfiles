{ inputs, config, pkgs, ... }:

{
  programs.fish = {
    enable = true;

    plugins = [
      {
        name = "nix-env";
        src = inputs.fish-plugin-nix-env;
      }
      {
        name = "pufferfish";
        src = inputs.fish-plugin-pufferfish;
      }
    ];

    # Other slightly less trivial functions are included directly as .fish files
    functions = {
      cheat = "curl cheat.sh/$argv[1]";
      jitsi = ''
        set url "https://meet.jit.si/$argv[1]"

        echo $url
        chromium --app=$url > /dev/null 2>/dev/null &; disown
      '';
    };

    # Somewhat nasty, but if fish is used as the login shell then that means
    # that the any environment variables configured through Home Manager will
    # only be set during login, and any chance requires you to log out. We'll
    # assume the changes are idempotent and we'll just always source the file
    # when logging in. This is the same way the environment variables are loaded
    # in the default fish init:
    # https://github.com/nix-community/home-manager/blob/2d963854ae2499193c0c72fd67435fee34d3e4fd/modules/programs/fish.nix#L357-L359
    interactiveShellInit = ''
      set -e __HM_SESS_VARS_SOURCED

      set --prepend fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d
      fenv source ${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh > /dev/null
      set -e fish_function_path[1]
    '';

    # The main configuration is done in `conf.d/conf.fish`
  };

  programs.starship = {
    enable = true;

    # The defaults mostly resemble Pure, but for some reason not quite. It's
    # also a bit too verbose for my liking
    settings = {
      character.success_symbol = "[❯](bold purple)";

      directory = {
        # There doesn't seem to be a way to do the actual fish-style truncation based
        # on the size of the window
        fish_style_pwd_dir_length = 1;
        truncation_length = 5;
        style = "blue";
      };

      cmd_duration = {
        format = "[$duration]($style) ";
        style = "bold yellow";
      };

      status = {
        disabled = false;
        format = "[$symbol$status( $signal_name)]($style)";
        map_symbol = true;
        pipestatus = true;
        pipestatus_format =
          "\\[$pipestatus\\] => [$symbol$status( $signal_name)]($style)";
      };

      sudo = {
        disabled = false;
        format = "[#]($style) ";
        style = "italic bright blue";
      };

      # This is super verbose since I'm always connected
      gcloud.disabled = true;
    };
  };

  # These aliases are portable, so may as well define them as such
  home.shellAliases = {
    # Enable colours and readable file sizes wherever applicable
    df = "df -h";
    du = "du -h";
    grep = "grep --color=auto";
    info = "info --vi-keys";

    # `programs.eza` can also define these but they're different form the ones I
    # prefer. It does already add `--git` and `--group-directories-first`.
    ls = "eza";
    ll = "ls --long";
    la = "ll -a";
    lt = "la -T";

    # Shorthand for interacting with the X11 clipboard (wl-clipboard provides
    # wl-copy and wl-paste so they don't need aliases)
    xsel = "xsel -b";
    xpaste = "xsel -o";
  };

  # Same idea for environment variables. Less trivial environment variables are
  # defined above.
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
    PAGER = "less";
    BROWSER = "mimeopen";
    LESS = "--RAW-CONTROL-CHARS --ignore-case --jump-target=4";

    # Always enable the fsync patches in Wine
    WINEFSYNC = 1;
    # Workaround for https://bugs.kde.org/show_bug.cgi?id=414785
    # https://www.reddit.com/r/kde/comments/h0u6j7/choppy_nvidia_x11_performance_on_plasma_519_with/ftoa0jv/
    # Doesn't actually fully resolve the issue, but it at least seems a tiny bit
    # better
    __GL_YIELD = "usleep";
  };

  services.gnome-keyring.enable = true;

  # These are a bit too verbose to include inline in `programs.fish.functions`
  xdg.configFile."fish/functions" = {
    source = ./functions;
    recursive = true;
  };
  xdg.configFile."fish/conf.d" = {
    source = ./conf.d;
    recursive = true;
  };
}
