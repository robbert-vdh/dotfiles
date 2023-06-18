{ pkgs, ... }:

{
  fonts.fontconfig.enable = true;

  # FIXME: For some reason this breaks bold text in Emacs, works fine with the
  #        repo provided version
  # home.packages = [ pkgs.jetbrains-mono ];

  xresources.extraConfig = ''
    ! FontConfig
    Xft.autohint: 0
    Xft.lcdfilter: lcddefault
    Xft.hintstyle: hintslight
    Xft.hinting: 1
    Xft.antialias: 1
    Xft.rgba: rgb
  '';

  xdg.configFile."fontconfig/conf.d" = {
    source = ./conf.d;
    recursive = true;
  };
}
