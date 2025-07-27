{ config, lib, pkgs, inputs, ... }: {
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

  services.mpdris2 = {
    enable = true;
    multimediaKeys = true;
    notifications = true;
  };
}
