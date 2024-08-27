{ config, pkgs, ... }: {
  imports = [
      ./hardware-configuration.nix
      ./grub.nix
      ./virt-manager.nix
      ./pipewire.nix
      ./bluetooth.nix
      ./brightness.nix
      ./zsh.nix
      ./graphics.nix
      ./virtualbox.nix
      ./printing.nix
      # ./xorg.nix
      # ./exwm.nix
      # ./sddm.nix
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  networking.hostName = "muh-laptop"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.networkmanager.enable = true;

  time.timeZone = "America/Boise";

  environment.variables = {
    PATH = "$PATH:$HOME/.local/bin/";
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Sway fixing
  # TODO put this in a flake
  security.polkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.cedar = {
    isNormalUser = true;
    description = "Cedar";
    extraGroups = [ "networkmanager" "wheel" "input" "video" ];
    initialPassword = "pass";
    packages = with pkgs; [];
  };

  # Enable doas
  security.doas.enable = true;
  security.sudo.enable = false;
  security.doas.extraRules = [{
    users = ["cedar"];
    keepEnv = true;
    persist = true;
  }];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    vim
    neovim
    git
    stow
    gcc
    gnugrep
    ripgrep
    wget
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
