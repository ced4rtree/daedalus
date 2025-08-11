{ config, lib, pkgs, ... }: {
  options.daedalus.shell.fish.enable = lib.mkEnableOption "fish";
  
  config = lib.mkIf config.daedalus.shell.fish.enable {
    programs.fish.enable = true;
    environment.shells = [ pkgs.fish ];
    users.users.cedar.shell = pkgs.fish;
  };
}
