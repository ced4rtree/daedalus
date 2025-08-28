{ config, lib, pkgs, inputs, ... }: {
  options.daedalus.home.programs.internet.discord.enable = lib.mkEnableOption "discord";

  imports = [
    inputs.nixcord.homeModules.nixcord
  ];

  config = lib.mkIf config.daedalus.home.programs.internet.discord.enable {
    programs.nixcord = {
      enable = true;
      discord.enable = false;
      vesktop.enable = true;
      config = {
        useQuickCss = true;
        plugins = {
          fakeNitro.enable = true;
          youtubeAdblock.enable = true;
        };
      };
    };
  };
}
