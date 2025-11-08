{ config, lib, ... }: {
  flake.modules.nixos.base = { pkgs, ... }: {
    imports = with config.flake.modules.nixos; [
      grub
      graphics
      plymouth
      pipewire
      bluetooth
      sudo
      sddm
      printing
      upower
      protonvpn
      nix
      networkManager
    ] ++ lib.attrVals (import ./_commonModules.nix) config.flake.modules.nixos;

    daedalus.protonvpn = {
      enable = true;
      autostart = false;
      interface.privateKeyFile = "/root/secrets/protonvpn";
      endpoint = {
        publicKey = "HkvZLLmIP39qmzFMCc7JHAquGTlh8Iphfc0WGbdjaQ8=";
        ip = "89.105.214.98";
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
    ];
  };
}
