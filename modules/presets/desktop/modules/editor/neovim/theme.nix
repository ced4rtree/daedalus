{ config, ... }: {
  flake.modules.nvf.theme = {
    vim.theme = {
      enable = true;
      name = "base16-pro-max";
      transparent = true;
      base16-colors = {
        inherit (config.flake.lib.stylix.colors.withHashtag)
        base00
        base01
        base02
        base03
        base04
        base05
        base06
        base07
        base08
        base09
        base0A
        base0B
        base0C
        base0D
        base0E
        base0F
        ;
      };
    };
  };
}
