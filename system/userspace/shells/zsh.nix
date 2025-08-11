{ config, pkgs, lib, ... }: {
  options.daedalus.shell.zsh.enable = lib.mkEnableOption "zsh";

  config = lib.mkIf config.daedalus.shell.zsh.enable {
    programs.zsh.enable = true;
    programs.zsh.syntaxHighlighting.enable = true;
    environment.shells = [ pkgs.zsh ];
    users.users.cedar.shell = pkgs.zsh;
  };
}
