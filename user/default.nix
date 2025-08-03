{ config, pkgs, ... }: {
  home.username = "cedar";
  home.homeDirectory = "/home/cedar";

  imports = [
    ./bar
    ./editor
    ./files
    ./launcher
    ./programs
    ./shell
    ./terminal
    ./wm
  ];

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

  home.sessionVariables = {
    NH_FLAKE = "$HOME/.dotfiles";
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
    tree-sitter
    nodejs_22
    gh
    openssh
    unzip
    mpv
    freetube
    qbittorrent
    clonehero
    appimage-run
    steam-run
    libtool
    graphviz
    edk2
    gimp
    xfce.thunar
    isync
    gnupg
    pinentry-qt
    lutris
    pass
    drawio
    texliveFull
    ispell
    steam
    wirelesstools
    ovito
    btop
    protonvpn-gui
  ];
}
