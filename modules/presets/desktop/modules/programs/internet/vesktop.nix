{ config, ... }: {
  flake.modules.nixos.discord = { pkgs, ... }: {
    hj.packages = [ pkgs.vesktop ];
  };

  # TODO A PR to nixcord that supports a hjem module should be possible, but
  # nixcord only supports home manager at the moment

  # flake-file.inputs.nixcord = {
  #   url = "github:KaylorBen/nixcord";
  #   inputs.nixpkgs.follows = "nixpkgs";
  # };

  # flake.modules.homeManager.discord = { inputs, ... }: {
  #   # stylix makes vesktop pretty unreadable
  #   stylix.targets.nixcord.enable = false;

  #   imports = [
  #     inputs.nixcord.homeModules.nixcord
  #   ];

  #   programs.nixcord = {
  #     enable = true;
  #     discord.enable = false;
  #     vesktop.enable = true;
  #     config = {
  #       useQuickCss = true;
  #       plugins = {
  #         fakeNitro.enable = true;
  #         youtubeAdblock.enable = true;
  #       };
  #     };
  #   };
  # };
}
