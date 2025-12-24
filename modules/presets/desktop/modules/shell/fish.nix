{ config, inputs, ... }: {
  flake.modules.nixos.fish = { pkgs, lib, ... }: let
    myPkgs = config.flake.packages.${pkgs.stdenv.hostPlatform.system};
  in {
    programs.fish.enable = true;
    environment.shells = [ myPkgs.fish ];
    users.users.${config.daedalus.username}.shell = myPkgs.fish;
  };

  perSystem = { pkgs, lib, system, ... }: {
    packages.fish = let
      colors = config.flake.lib.stylix.createColorsFrom pkgs;
      theme = colors {
        templateRepo = inputs.stylix.inputs.base16-fish;
      };
      fishConf = pkgs.writeText "fish-conf" ''
        set -U fish_user_paths $HOME/.local/bin/ $fish_user_paths
        set -U fish_greeting
  
        source ${theme}
        # See https://github.com/tomyun/base16-fish/issues/7 for why this condition exists
        if status --is-interactive && test -z "$TMUX"
          base16-${colors.slug}
        end
  
        if status is-interactive
          # call my patched version of microfetch
          ${lib.getExe config.flake.packages.${system}.microfetch}
        end

        ${lib.getExe config.flake.packages.${system}.starship} init fish | source
      '';
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.fish;
      flags."-C" = "source ${fishConf}";
    };
  };
}
