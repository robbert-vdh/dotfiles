{ pkgs, ... }:

{
  home.packages = [ pkgs.tmux ];

  xdg.configFile."tmux/tmux.conf".source = ./tmux.conf;
}
