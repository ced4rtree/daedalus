{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.cli.gpg.enable = lib.mkEnableOption "gpg";

  config = lib.mkIf config.daedalus.home.programs.cli.gpg.enable {
    home.packages = with pkgs; [
      gnupg
      pinentry-qt
    ];

    home.file.".gnupg/gpg.conf".text = ''
      use-agent
      pinentry-mode loopback
    '';
  };
}
