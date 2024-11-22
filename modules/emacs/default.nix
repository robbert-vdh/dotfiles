{ config, lib, pkgs, mkAbsoluteSymlink, ... }:

let
  # Just to make sure everything is in sync
  emacsPkg = pkgs.emacs30;
  configDir = ./doom;

  # Based on
  # https://github.com/nix-community/nix-doom-emacs/issues/60#issuecomment-1083630633.
  # This makes sure the `config.org` file is tangled correctly. Doom normally
  # does this on `doom sync`. This version doesn't depend on the Doom CLI, but
  # it does require the `config.org` file to be annotated with `#+PROPERTY:
  # header-args:emacs-lisp :tangle yes`.
  # FIXME: Try this again at one point. We're now just symlinking ~/.config/doom
  #        directly to this repo because otherwise things break
  # tangledDoomConfig = pkgs.stdenv.mkDerivation {
  #   pname = "literate-emacs-config";
  #   version = "dev";

  #   src = configDir;
  #   dontUnpack = true;

  #   nativeBuildInputs = [ emacsPkg ];

  #   buildPhase = ''
  #     cp -r $src/* .
  #     emacs --batch -Q \
  #       -l org \
  #       config.org \
  #       -f org-babel-tangle
  #   '';

  #   installPhase = ''
  #     mkdir -p "$out"
  #     # This needs to include everything from `./doom`, plus the generated
  #     # config.el file
  #     cp -r * "$out"
  #   '';
  # };
in {
  home.packages = [
    emacsPkg

    # Needed for the vterm package. Magically works when you add this.
    pkgs.emacsPackages.vterm
  ];

  # Not really used anymore, but this was once useful in conjunction with GNU
  # Global to get tags in .scss files
  home.sessionVariables.GTAGSLABEL = "pygments";

  home.file.".globalrc".source = ./.globalrc;

  xdg.configFile."doom" = {
    # See above. This is a mutable symlink so Doom can be used imperatively
    # without things breaking.
    source = mkAbsoluteSymlink config "modules/emacs/doom";
    # This keeps ~/.config/doom writable, although the individual files cannot
    # be overwritten
    recursive = true;
  };
}
