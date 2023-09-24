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
    modules/haskell
    modules/kde
    modules/mangohud
    modules/mpv
    modules/nix
    modules/pacman
    modules/pipewire
    modules/python
    modules/tex
    modules/tmux
    modules/vim
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
  home.stateVersion = "23.11"; # Please read the comment before changing.

  # This option is broken right now:
  # https://github.com/nix-community/home-manager/issues/2942#issuecomment-1119760100
  # nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = (pkg: true);

  # The home.packages option allows you to install Nix packages into your
  # environment.
  #
  # Many of the modules imported above also add their own list of packages
  home.packages = [
    pkgs.fd
    pkgs.httpie
    pkgs.inotify-tools
    pkgs.nixfmt
    pkgs.overmind
    pkgs.pre-commit
    pkgs.ripgrep
    pkgs.websocat
    pkgs.wl-clipboard
    pkgs.xsel

    # Linters, LISP servers, editor integration things
    pkgs.html-tidy
    pkgs.nodePackages.bash-language-server
    pkgs.nodePackages.prettier
    pkgs.nodePackages.pyright
    pkgs.nodePackages.vscode-json-languageserver
    pkgs.nodePackages.yaml-language-server
    pkgs.rnix-lsp
    pkgs.shellcheck
    pkgs.shfmt

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
  programs.eza = {
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
