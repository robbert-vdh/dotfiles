{ pkgs, ... }:

{
  xdg.configFile."latexmk/latexmkrc".source = ./latexmkrc;
}
