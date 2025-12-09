{
  flake.modules.nixos.ly = {
    services.displayManager.ly = {
      enable = true;
      settings = {
        animation = "doom";
        bigclock = "en";
      };
    };
  };
}
