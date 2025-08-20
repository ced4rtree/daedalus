{ config, pkgs, self, mylib, ... }: {
  imports = mylib.scanPaths ./.;

  networking.hostName = config.daedalus.host.hostname;
  networking.networkmanager.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cedar = {
    isNormalUser = true;
    description = "Cedar";
    extraGroups = [ "networkmanager" "wheel" "input" "video" ];
    initialPassword = "pass";
  };

  # Full system module declaration
  daedalus = {
    bootloader = "grub";
    graphics.enable = true;
    plymouth.enable = true;
    audio.pipewire.enable = true;
    bluetooth.enable = true;
    security.sudo.enable = true;
    displayManager = "sddm";
    printing.enable = true;
    shell.zsh.enable = true;
    upower.enable = true;
    wm.hyprland.enable = true;
    stylix.enable = true;
  };

  environment.systemPackages = with pkgs; [
    neovim
    git
    gnugrep
    ripgrep
    wget
    openssh
    unzip
    nh
  ];

  # Don't change unless you need to
  system.stateVersion = "24.05";
}
