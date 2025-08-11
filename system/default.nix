{ config, pkgs, self, ... }: {
  imports = [
    ./core
    ./userspace
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" "pipe-operators" ];

  networking.hostName = "muh-laptop";
  networking.networkmanager.enable = true;

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

  nix.settings.warn-dirty = false;

  environment.systemPackages = with pkgs; [
    neovim
    git
    gnugrep
    ripgrep
    wget
    openssh
    unzip
    nh
    doas-sudo-shim
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

}
