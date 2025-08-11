{ config, lib, pkgs, ... }: {
  # the standard option stuff isn't being used for now due to wanting this to
  # rely on the system virt-manager option, which hasn't been written yet.
  # config = lib.mkIf config.daedalus.desktop.virt-manager.enable {
    dconf.settings = {
      "org/virt-manager/virt-manager/connections" = {
        autoconnect = ["qemu:///system"];
        uris = ["qemu:///system"];
      };
    };
  # };
}
