{ config, inputs, ... }: {
  flake.modules.nixos.mako = { pkgs, ... }: let
    mako = config.flake.packages.${pkgs.stdenv.hostPlatform.system}.mako;
  in {
    hj.packages = [ mako ];
    dbus.package = [ mako ];
  };

  perSystem = { pkgs, lib, ... }: {
    packages.mako = (inputs.wrappers.wrapperModules.mako.apply {
      inherit pkgs;
      settings = {
        default-timeout = 5000; # 5 seconds
        ignore-timeout = false;

        on-notify = "exec ${lib.getExe pkgs.mpv} /run/current-system/sw/share/sounds/freedesktop/stereo/message.oga";
      };
    }).wrapper;
  };
}
