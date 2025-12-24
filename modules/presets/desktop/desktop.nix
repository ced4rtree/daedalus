{ config, lib, ... }: {
  daedalus = {
    terminal = "kitty";
    windowManager = "hyprland";
  };

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
      hjem

      windowManager
      zsh
      desktopPrograms
      splash
      gnome-polkit
      
      terminal
      hypridle
      files
      doomEmacs
      neovim
      noctalia
      fuzzel
      hyprpaper
    ];

    environment.systemPackages = with pkgs; [
      htop
      floorp-bin
      gh
      mpv
      qbittorrent
      clonehero
      appimage-run
      steam-run
      gimp
      xfce.thunar
      isync
      pass
      drawio
      ispell
      steam
      wirelesstools
      btop
    ];

    daedalus.isDesktop = true;
  };
}
