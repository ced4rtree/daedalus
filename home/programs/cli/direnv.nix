{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.cli.direnv.enable = lib.mkEnableOption "direnv";

  config = lib.mkIf config.daedalus.home.programs.cli.direnv.enable {
    programs.direnv = {
      enable = true;
      enableZshIntegration = config.daedalus.home.shell.zsh.enable;
      nix-direnv.enable = true;
    };
  };
}
