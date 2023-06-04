{ pkgs, ... }:

{
  home.packages = [
    pkgs.haskellPackages.implicit-hie # Useful to have available globally of specific projects
    pkgs.haskellPackages.ghc-events # Useful for investigating RTS event logs
  ];

  xdg.configFile."fourmolu.yaml".source = ./fourmolu.yaml;
}
