{
  flake.modules.nixos.upower = {
    upower.enable = true;
    power-profiles-daemon.enable = true;
  };
}
