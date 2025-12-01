{
  flake.modules.homeManager.fuzzel = { config, ... }: {
    programs.fuzzel = {
      enable = true;
      settings = {
        main = {
          fields = "filename,name,generic,keywords,comment,categories";
          terminal = "${config.daedalus.terminalCommand} -e";
          anchor = "center";
          width = 75;
        };

        border = {
          width = 2;
          radius = 0;
        };
      };
    };
  };
}
