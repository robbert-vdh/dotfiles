{ lib, pkgs, ... }:

# This depends on the `nix-doom-emacs` module
let
  # Just to make sure everything is in sync
  emacsPkg = pkgs.emacs;
  configDir = ./doom;

  # Based on
  # https://github.com/nix-community/nix-doom-emacs/issues/60#issuecomment-1083630633.
  # There's no included option to tangle a .org file, so we need a custom
  # derivation.
  tangled-emacs-config = pkgs.stdenv.mkDerivation {
    pname = "emacs-config";
    version = "dev";

    src = configDir;
    dontUnpack = true;

    buildInputs = [ emacsPkg ];

    buildPhase = ''
      cp -r $src/* .
      # Doom normally does this on `doom sync`. This version doesn't depend on
      # the Doom CLI, but it does require the `config.org` file to be annotated
      # with `#+PROPERTY: header-args:emacs-lisp :tangle yes`.
      emacs --batch -Q \
        -l org \
        config.org \
        -f org-babel-tangle
    '';

    installPhase = ''
      install -D -t $out *.el
    '';
  };
in {
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = tangled-emacs-config;
    emacsPackage = emacsPkg;
  };

  home.file.".globalrc".source = ./.globalrc;
}
