{ config, pkgs, ... }: {
  home.packages = with pkgs; [
    # qemu_full
  ];

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
}
