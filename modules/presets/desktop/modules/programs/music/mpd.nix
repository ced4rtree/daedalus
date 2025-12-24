{ config, inputs, ... }: let
  topConfig = config;
in {
  flake.modules.nixos.mpd = { config, lib, pkgs, ... }: {
    hj.packages = [ pkgs.mpc ];

    users.users.${topConfig.daedalus.username}.extraGroups = [ "mpd" ];

    hj.systemd.services.mpd = {
      description = "Music Player Daemon";
      after = [
        "network.target"
        "sound.target"
      ];

      wantedBy = [ "default.target" ];

      serviceConfig = let
        dataDir = "~/.local/share/mpd";
        mpdConf = pkgs.writeText "mpd.conf" ''
          music_directory "~/Music"
          
          db_file "${dataDir}/tag_cache"
          state_file "${dataDir}/state"
          sticker_file "${dataDir}/sticker.sql"

          bind_to_address "127.0.0.1"

          audio_output {
            type "pipewire"
            name "Pipewire Output"
          }
        '';
      in {
        ExecStart = "${pkgs.mpd}/bin/mpd --no-daemon ${mpdConf}";
        Type = "notify";
        ExecStartPre = ''${pkgs.bash}/bin/bash -c "${pkgs.coreutils}/bin/mkdir -p '${dataDir}'"'';
      };
    };

    hj.systemd.services.mpdris2 = {
      wantedBy = [ "default.target" ];
      description = "MPRIS 2 support for MPD";
      after = [ "mpd.service" ];

      serviceConfig = {
        Type = "simple";
        Restart = "on-failure";
        RestartSec = "5s";
        ExecStart = lib.getExe topConfig.flake.packages.${pkgs.stdenv.hostPlatform.system}.mpdris2;
        BusName = "org.mpris.MediaPlayer2.mpd";
      };
    };
  };

  perSystem = { pkgs, lib, ... }: {
    packages.mpdris2 = let
      toIni = lib.generators.toINI {
        mkKeyValue = key: value: let
          value' = if lib.isBool value
          then (if value then "True" else "False")
          else toString value;
        in "${key} = ${value'}";
      };

      mpdrisConf = pkgs.writeText "mpdris.ini" (toIni {
        Bling = {
          notify = true;
          mmkeys = true;
        };
      });
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.mpdris2;

      flags = {
        "-c" = "${mpdrisConf}";
      };
    };
  };
}
