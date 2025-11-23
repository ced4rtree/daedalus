{ config, ... }: {
  flake.modules.nixos.fish = { pkgs, lib, ... }: {
    programs.fish.enable = true;
    environment.shells = [ pkgs.fish ];
    users.users.${config.daedalus.username}.shell = pkgs.fish;
  };

  flake.modules.homeManager.fish = { pkgs, ... }: let
    catppuccin-fish = pkgs.fetchFromGitHub {
      owner = "catppuccin";
      repo = "fish";
      rev = "0ce27b518e8ead555dec34dd8be3df5bd75cff8e";
      hash = "sha256-Dc/zdxfzAUM5NX8PxzfljRbYvO9f9syuLO8yBr+R3qg=";
    };
  in {
    imports = [
      config.flake.modules.homeManager.starship
    ];

    xdg.configFile."fish/themes/Catppuccin Mocha.theme".source = "${catppuccin-fish}/themes/Catppuccin Mocha.theme";

    programs.fish = {
      enable = true;

      shellInit = ''
        set -U fish_user_paths $HOME/.local/bin/ $fish_user_paths
        set -U fish_greeting

        fish_config theme choose "Catppuccin Mocha"

        if status is-interactive
          fastfetch
        end
      '';
    };
  };
}
