{ config, lib, pkgs, ... }:

{
  xresources.extraConfig = ''
    ! FontConfig
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintslight
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';

  xdg.configFile."fonts" = {
    source = ./conf;
    # This keeps ~/.config/doom writable, although the individual files cannot
    # be overwritten
    recursive = true;
  };
}
