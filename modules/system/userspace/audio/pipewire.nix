{ config, ... }: let
  username = config.daedalus.username;
in {
  flake.modules.nixos.pipewire = { config, lib, pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      pulseaudio
      pavucontrol
      alsa-utils
    ];

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      socketActivation = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;
      systemWide = true;
      audio.enable = true;
    };

    users.users.${username}.extraGroups = [ "pipewire" "audio" ];

    services.pipewire.wireplumber.extraConfig = lib.mkIf config.hardware.bluetooth.enable {
      "monitor.bluez.properties" = {
        "bluez5.enable-sbc-xq" = true;
        "bluez5.enable-msbc" = true;
        "bluez5.enable-hw-volume" = true;
        "bluez5.roles" = [ "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" ];
      };
    };
  };
}
