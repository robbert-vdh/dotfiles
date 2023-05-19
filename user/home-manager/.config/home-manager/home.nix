{ config, pkgs, ... }:

# These Python packages aren't packaged in nixpkgs
let mkJupyterlab_code_formatter = ps: with ps; (buildPythonPackage rec {
      pname = "jupyterlab_code_formatter";
      version = "2.0.0";
      format = "pyproject";

      src = fetchPypi {
        inherit pname version;
        sha256 = "gGWqAPLmes6PqdKLYq1NPow23uL3TlOg0SKkHq0ixew=";
      };

      buildInputs = [
        hatchling
        hatch-jupyter-builder
        hatch-nodejs-version
        jupyterlab
      ];
    });
in
{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "robbert";
  home.homeDirectory = "/home/robbert";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "22.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # The pure-GTK option should allow for better Wayland compatibility. In
    # theory.
    (pkgs.emacs28.override { withPgtk = true; })

    # Important tools. I'm not installing fish through Nix since I'm using it as
    # my login shell, and it needs some config to know about Nix.
    pkgs.direnv
    pkgs.git

    # Useful to have available globally of specific projects
    pkgs.haskellPackages.implicit-hie

    # Lesser important tools
    pkgs.bat
    pkgs.delta
    pkgs.exa
    pkgs.fd
    pkgs.fzf
    pkgs.gh
    pkgs.httpie
    pkgs.ripgrep
    pkgs.shellcheck
    pkgs.websocat

    ((pkgs.python310.override {
      packageOverrides = self: super: {
        # Neither Qt nor TK works out of the box. I figured out a workaround to
        # get the Qt backend to work involving the `qt5-run` wrapper defined
        # below, so that's what I'm using for now.
        matploblib = super.matplotlib.override { enableQt = true; enableTk = false; };
      };
    }).withPackages (ps: with ps; [
      # Binaries
      ipython
      jupyterlab

      # Formatters and linters
      black
      (mkJupyterlab_code_formatter ps)
      isort

      # Libs
      librosa
      matplotlib
      numpy
      pandas
      pyqt5 # Needed for matplotlib
      scipy
      seaborn
      statsmodels
    ]))

    pkgs.haskellPackages.ghc-events # Useful for investigating RTS event logs

    pkgs.nodePackages.bash-language-server

    # HACK: Workaround needed to set the correct Qt plugin path so matplotlib
    #       works in IPython:
    #       https://github.com/NixOS/nixpkgs/issues/80147#issuecomment-784857897
    (pkgs.writeShellScriptBin "qt5-run" (with pkgs.qt5; ''
      export QT_PLUGIN_PATH=${qtbase}/${qtbase.qtPluginPrefix}
      if [[ $# -eq 0 ]]; then
        echo >&2 "Usage: qt5-run <command...>"
        exit 1
      fi

      exec "$@"
    ''))

    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/robbert/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
