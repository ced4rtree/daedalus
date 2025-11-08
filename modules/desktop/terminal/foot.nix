{
  flake.modules.homeManager.foot = {
    programs.foot = {
      enable = true;
      settings = {
        main.term = "xterm-256color";
        scrollback.multiplier = 7.0;
      };
    };
  };
}
