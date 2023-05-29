{ config, lib, pkgs, ... }:

{
  xdg.configFile."pipewire" = {
    source = ./pipewire;
    recursive = true;
  };

  xdg.configFile."wireplumber" = {
    source = ./wireplumber;
    recursive = true;
  };
}
