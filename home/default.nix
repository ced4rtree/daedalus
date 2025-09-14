{ config, lib, pkgs, mylib, ... }: {
  home.username = "cedar";
  home.homeDirectory = "/home/cedar";

  imports = mylib.scanPaths ./.;

  # Don't change unless you need to
  home.stateVersion = "24.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # TODO: put nh stuff in a module
  home.sessionVariables = {
    NH_FLAKE = "$HOME/.dotfiles";
  };

  # desktop configuration
  daedalus.home = {
    stylix.enable = true;
    bar.waybar.enable = true;
    editor.emacs = {
      enable = true;
      flavor = "doom";
    };
    wallpaper.swww.enable = true;
    launcher.fuzzel.enable = true;
    shell.fish.enable = true;
    terminal.foot.enable = true;
    wm.niri.enable = true;
    idle.hypridle.enable = true;
    lock.hyprlock.enable = true;
    notifications.mako.enable = true;

    programs = {
      cli = {
        direnv.enable = true;
        gpg.enable = true;
      };

      desktop = {
        batsignal.enable = true;
        homestuck.enable = true;
        xdg.enable = true;
      };

      internet = {
        qutebrowser.enable = true;
        nyxt.enable = false;
        discord.enable = true;
      };

      music = {
        mpd.enable = false;
        mpris.enable = true;
      };
    };
  };

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # fonts
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    nerd-fonts.ubuntu-sans
    hasklig

    # misc
    htop
    floorp
    gh
    mpv
    freetube
    qbittorrent
    clonehero
    appimage-run
    steam-run
    gimp
    xfce.thunar
    isync
    lutris
    pass
    drawio
    ispell
    steam
    wirelesstools
    btop
    protonvpn-gui
  ];
}
