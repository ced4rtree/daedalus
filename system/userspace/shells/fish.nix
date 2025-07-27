{ config, pkgs, ... }: {
  programs.fish.enable = true;
  environment.shells = [ pkgs.fish ];
  users.users.cedar.shell = pkgs.fish;
}
