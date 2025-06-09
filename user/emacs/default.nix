{ lib, config, pkgs, ... }: let
  emacs = pkgs.emacs30-pgtk;
in {
  services.emacs = {
    enable = true;
    package = emacs;
    defaultEditor = true;
  };

  programs.emacs = {
    enable = true;
    package = emacs;
    extraConfig = (builtins.readFile ./init.el);

    extraPackages = epkgs: [
      pkgs.mu
      epkgs.mu4e
      pkgs.emacsPackages.mu4e
    ];
  };
}
