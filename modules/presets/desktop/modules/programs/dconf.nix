{
  flake.modules.nixos.dconf = { pkgs, lib, config, ... }: {
    options.daedalus.dconf = lib.mkOption {
      type = lib.types.attrs;
      description = "Dconf settings to apply";
      default = { };
      example = lib.literalExpression ''
        "org/virt-manager/virt-manager/connections" = {
          autoconnect = ["qemu:///system"];
          uris = ["qemu:///system"];
        };
      '';
    };

    config.system.userActivationScripts.dconfSettings = let
      dconfConfig = pkgs.writeText
        "dconf.ini"
        (lib.generators.toDconfINI config.daedalus.dconf);
    in {
      text = "${lib.getExe pkgs.dconf} load / < ${dconfConfig}";
      deps = [];
    };
  };
}
