{ config, pkgs, ... }: {
  environment.systemPackages = [(
    pkgs.catppuccin-sddm.override {
      flavor = "frappe";
      font = "JetBrainsMono Nerd Font";
      fontSize = "15";
    }
  )];

  services.displayManager.sddm = {
    enable = true;
    theme = "catppuccin-frappe";
    package = pkgs.kdePackages.sddm;
  };
}
