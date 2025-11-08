{
  flake.modules.nixos.upower = {
    services = {
      upower.enable = true;
      power-profiles-daemon.enable = true;
    };
  };
}
