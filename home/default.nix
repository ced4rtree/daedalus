{ config, lib, pkgs, mylib, ... }: {
  home.username = "cedar";
  home.homeDirectory = "/home/cedar";

  imports = mylib.scanPaths ./.;

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.05"; # Please read the comment before changing.

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
    editor.emacs.enable = true;
    wallpaper.swww.enable = true;
    launcher.fuzzel.enable = true;
    shell.zsh.enable = true;
    terminal.foot.enable = true;
    wm.hyprland.enable = true;
    idle.hypridle.enable = true;
    lock.hyprlock.enable = true;

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
