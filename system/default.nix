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
    shell.fish.enable = true;
    upower.enable = true;
    wm.niri.enable = true;
    stylix.enable = true;
    protonvpn = {
      enable = true;
      autostart = false;
      interface.privateKeyFile = "/root/secrets/protonvpn";
      endpoint = {
        publicKey = "HkvZLLmIP39qmzFMCc7JHAquGTlh8Iphfc0WGbdjaQ8=";
        ip = "89.105.214.98";
      };
    };
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
