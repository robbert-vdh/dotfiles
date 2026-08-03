{ config, lib, pkgs, ... }:

{
  xdg.configFile."wireplumber" = {
    source = ./wireplumber;
    recursive = true;
  };
}
