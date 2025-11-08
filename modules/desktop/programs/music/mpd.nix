{
  flake.modules.homeManager.mpd = { config, lib, pkgs, ... }: {
    home.packages = [ pkgs.mpc ];

    services.mpd = {
      enable = true;
      extraConfig = ''
        audio_output {
          type "pipewire"
          name "Pipewire Output"
        }
      '';
    };

    services.mpdris2 = lib.mkIf config.services.mpris-proxy.enable {
      enable = true;
      multimediaKeys = true;
      notifications = true;
    };
  };
}
