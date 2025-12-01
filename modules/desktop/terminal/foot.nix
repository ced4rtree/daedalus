{ config, ... }: {
  flake.modules.homeManager.foot = {
    imports = [ config.flake.modules.homeManager.terminalOption ];

    daedalus.terminalCommand = "foot";

    programs.foot = {
      enable = true;
      settings = {
        main.term = "xterm-256color";
        scrollback.multiplier = 7.0;
      };
    };
  };
}
