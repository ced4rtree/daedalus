{ config, lib, pkgs, ... }: {
  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "JetBrainsMono Nerd Font:size=10";
        fields = "filename,name,generic,keywords,comment,categories";
        terminal = "footclient -e";
        anchor = "center";
        width = 75;
      };

      border = {
        width = 2;
        radius = 0;
      };

      # catppuccin frappe blue
      colors = {
        background="303446ff";
        text="c6d0f5ff";
        prompt="b5bfe2ff";
        placeholder="838ba7ff";
        input="c6d0f5ff";
        match="8caaeeff";
        selection="626880ff";
        selection-text="c6d0f5ff";
        selection-match="8caaeeff";
        counter="838ba7ff";
        border="8caaeeff";
      };
    };
  };
}
