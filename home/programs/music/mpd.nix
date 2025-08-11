{ config, lib, pkgs, inputs, ... }: {
  options.daedalus.home.programs.music.mpd.enable = lib.mkEnableOption "mpd";

  config = lib.mkIf config.daedalus.home.programs.music.mpd.enable {
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

    services.mpdris2 = lib.mkIf config.daedalus.home.programs.music.mpris.enable {
      enable = true;
      multimediaKeys = true;
      notifications = true;
    };
  };
}
