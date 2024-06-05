{ config, pkgs, ... }: {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "cedar";
  home.homeDirectory = "/home/cedar";

  imports = [
    ./virt-manager.nix
    ./zsh.nix
    ./wm/sway.nix
    ./bluetooth.nix
    ./theme.nix
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

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "JetBrainsMono" "Iosevka" ]; })
    htop
    brave
    firefox-unwrapped
    tree-sitter
    nodejs_22
    emacs29-pgtk
    mu
	emacsPackages.mu4e
    gh
    openssh
    unzip
    mpv
    light
    freetube

    # music
    mpd
    ncmpcpp
    mpc-cli

    # dev tools
    cmake
    gnumake

    wirelesstools
  ];

  programs.emacs = {
    extraPackages = epkgs: [
      pkgs.mu
	  epkgs.mu4e
    ];
  };
}
