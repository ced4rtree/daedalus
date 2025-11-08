{
  flake.modules.homeManager.nyxt = {
    programs.nyxt = {
      enable = true;
      config = builtins.readFile ./config.lisp;
    };
  };
}
