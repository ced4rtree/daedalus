{
  flake.modules.nixos.gnome-polkit = { pkgs, ... }: {
    security.polkit.enable = true;
  };

  flake.modules.homeManager.gnome-polkit = {
    services.polkit-gnome.enable = true;
  };
}
