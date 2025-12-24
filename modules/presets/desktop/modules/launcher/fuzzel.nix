{ config, inputs, ... }: {
  flake.modules.nixos.fuzzel = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.fuzzel ];
  };

  perSystem = { pkgs, lib, ... }: {
    packages.fuzzel = (inputs.wrappers.wrapperModules.fuzzel.apply {
      inherit pkgs;

      settings = {
        main = {
          fields = "filename,name,generic,keywords,comment,categories";
          terminal = "${config.daedalus.terminal.command} -e";
          anchor = "center";
          width = 75;
          font = with config.flake.lib.stylix.fonts; "${sansSerif.name}:size=${toString sizes.popups}";
        };

        border = {
          width = 2;
          radius = 0;
        };

        # taken straight from stylix source code
        colors = let
          inherit (config.flake.lib.stylix) opacity;
          opacity' = lib.toHexString (builtins.ceil (opacity.popups * 255));
        in with config.flake.lib.stylix.colors; {
          background = "${base00-hex}${opacity'}";
          text = "${base05-hex}ff";
          placeholder = "${base03-hex}ff";
          prompt = "${base05-hex}ff";
          input = "${base05-hex}ff";
          match = "${base0A-hex}ff";
          selection = "${base03-hex}ff";
          selection-text = "${base05-hex}ff";
          selection-match = "${base0A-hex}ff";
          counter = "${base06-hex}ff";
          border = "${base0D-hex}ff";
        };
      };
    }).wrapper;
  };
}
