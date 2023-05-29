{ lib, pkgs, ... }:

# FIXME: Fetch Doom from non-flake git, install
let
  # Just to make sure everything is in sync
  emacsPkg = pkgs.emacs;
  configDir = ./doom;

  # Based on
  # https://github.com/nix-community/nix-doom-emacs/issues/60#issuecomment-1083630633.
  # This makes sure the `config.org` file is tangled correctly. Doom normally
  # does this on `doom sync`. This version doesn't depend on the Doom CLI, but
  # it does require the `config.org` file to be annotated with `#+PROPERTY:
  # header-args:emacs-lisp :tangle yes`.
  tangledDoomConfig = pkgs.stdenv.mkDerivation {
    pname = "literate-emacs-config";
    version = "dev";

    src = configDir;
    dontUnpack = true;

    buildInputs = [ emacsPkg ];

    buildPhase = ''
      cp -r $src/* .
      emacs --batch -Q \
        -l org \
        config.org \
        -f org-babel-tangle
    '';

    installPhase = ''
      mkdir -p "$out"
      # This needs to include everything from `./doom`, plus the generated
      # config.el file
      cp -r * "$out"
    '';
  };
in {
  home.packages = [ emacsPkg ];

  home.file.".globalrc".source = ./.globalrc;
  xdg.configFile."doom".source = tangledDoomConfig;
}
