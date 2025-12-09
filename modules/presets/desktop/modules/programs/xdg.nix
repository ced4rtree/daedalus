{
  flake.modules.homeManager.xdg = { config, lib, pkgs, ... }: {
    xdg = {
      mime.enable = true;
      userDirs.enable = true;
    };
  };
}
