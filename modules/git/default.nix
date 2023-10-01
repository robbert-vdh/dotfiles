{ pkgs, ... }:

{
  programs.gh.enable = true;

  programs.git = {
    enable = true;

    # Fancy syntax highlighting and word diffing. Also used in Emacs through
    # `magit-delta`.
    delta.enable = true;
    lfs.enable = true;

    userName = "Robbert van der Helm";
    userEmail = "mail@robbertvanderhelm.nl";

    aliases = {
      lg =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=human";
      lga = "log --graph --date=human --all";
    };

    extraConfig = {
      github.user = "robbert-vdh";
      gitlab.user = "robbert-vdh";

      commit.verbose = true;
      diff.algorithm = "histogram";
      init.defaultBranch = "master";
      merge.conflictStyle = "zdiff3";
      pull.rebase = true;
      rebase.autoStash = true;

      color.ui = true;
      log.date = "human-local";
      magit.hideCampaign = true;
      sendmail.confirm = "always";
    };

    ignores = [
      ".gdb_history"
      ".ipynb_checkpoints"
      ".tern-port"

      # AUCTeX
      "*.synctex.gz"
      ".auctex-auto"
      "_region_.prv"
      "_region_.tex"
      "prv_*.fmt"

      # CMake/clangd/ccls
      ".cache/clangd"
      ".ccls-cache/"
      ".ccls"
      ".clangd"
      "compile_commands.json"

      # Emacs
      ".dir-locals.el"
      ".project"
      ".projectile"

      # Haskell
      "hie.yaml"

      # IntelliJ
      ".idea"

      # PHP
      "dump.php"
      "pimple.json"

      # Tags
      ".ac-php-conf.json"
      "GPATH"
      "GRTAGS"
      "GTAGS"
      "gtags.files"

      # VsCode
      ".vscode"
      "!user/vscode/.vscode"

      # Direnv
      ".envrc"
    ];
  };
}
