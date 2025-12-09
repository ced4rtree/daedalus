{ config, lib, ... }: {
  flake.modules.nixos.desktop = { pkgs, ... }: {
    imports = with config.flake.modules.nixos; [
      graphics
      plymouth
      pipewire
      bluetooth
      sddm
      printing
      upower
      protonvpn
    ] ++ (lib.attrVals (import ./_commonModules.nix) config.flake.modules.nixos);

    daedalus.isDesktop = true;

    daedalus.protonvpn = {
      enable = true;
      autostart = false;
      interface.privateKeyFile = "/root/secrets/protonvpn";
      endpoint = {
        publicKey = "HkvZLLmIP39qmzFMCc7JHAquGTlh8Iphfc0WGbdjaQ8=";
        ip = "89.105.214.98";
      };
    };
  };
}
