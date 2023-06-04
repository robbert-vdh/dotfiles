{ config, pkgs, username, ... }:

{
  # These modules all come with config files. Anything simple that doesn't
  # require config files or custom package definitions is included directly in
  # this file.
  imports = [
    modules/emacs
    modules/fish
    modules/fonts
    modules/gdb
    modules/git
    modules/nix
    modules/pacman
    modules/pipewire
    modules/python
  ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = username;
  home.homeDirectory = "/home/${username}";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "22.11"; # Please read the comment before changing.

  # This option is broken right now:
  # https://github.com/nix-community/home-manager/issues/2942#issuecomment-1119760100
  # nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (pkg: true);

  # The home.packages option allows you to install Nix packages into your
  # environment.
  #
  # Many of the modules imported above also add their own list of packages
  home.packages = [
    # Useful to have available globally of specific projects
    pkgs.haskellPackages.implicit-hie

    # Lesser important tools
    pkgs.fd
    pkgs.html-tidy
    pkgs.httpie
    pkgs.nixfmt
    pkgs.pre-commit
    pkgs.ripgrep
    pkgs.rnix-lsp
    pkgs.shellcheck
    pkgs.websocat
    pkgs.wl-clipboard
    pkgs.xsel

    pkgs.haskellPackages.ghc-events # Useful for investigating RTS event logs

    pkgs.nodePackages.bash-language-server
    pkgs.nodePackages.prettier
    pkgs.nodePackages.pyright
    pkgs.nodePackages.yaml-language-server

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

  programs.bat.enable = true;
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.exa = {
    enable = true;
    git = true;
    extraOptions = [ "--group-directories-first" ];
  };
  programs.fzf.enable = true;
  programs.jq.enable = true;

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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
