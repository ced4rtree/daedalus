{ config, ... }: {
  flake.modules.homeManager.discord = { inputs, ... }: {
    # stylix makes vesktop pretty unreadable
    stylix.targets.nixcord.enable = false;

    imports = [
      inputs.nixcord.homeModules.nixcord
    ];

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
