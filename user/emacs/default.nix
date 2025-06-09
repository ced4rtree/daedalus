{ lib, config, pkgs, ... }: let
  emacs = pkgs.emacs30-pgtk;
in {
  home.packages = with pkgs; [
    emacsPackages.mu4e
    mu
  ];
  
  services.emacs = {
    enable = true;
    package = emacs;
    defaultEditor = true;
  };

  programs.emacs = {
    enable = true;
    package = emacs;
    extraConfig = (builtins.readFile ./init.el);
  };

  home.file.".config/emacs/init.el".source = ./init.el;
  home.file.".config/emacs/early-init.el".source = ./early-init.el;
}
