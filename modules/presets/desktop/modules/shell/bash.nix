{ config, inputs, ... }: {
  flake.modules.nixos.bash = { pkgs, ... }: let
    bash = config.flake.packages.${pkgs.stdenv.hostPlatform.system}.bash;
  in {
    programs.bash.enable = true;
    environment.shells = [ bash ];
    users.users.${config.daedalus.username}.shell = bash;
  };

  perSystem = { pkgs, lib, ... }: let
    bashConfig = pkgs.writeText "bash-config" ''
      function parse_git_branch() {
        BRANCH=`git branch 2>/dev/null | grep '^\*' | colrm 1 2`
        if [ ! $BRANCH == "" ]; then
          echo " $BRANCH"
        else
          echo ""
        fi
      }
      
      export PS1="\[\e[34m\]\w\[\e[32m\]\`parse_git_branch\` \[\e[35m\]->\[\e[0m\] "

      ${lib.getExe config.flake.packages.${pkgs.stdenv.hostPlatform.system}.microfetch}
    '';
  in {
    packages.bash = inputs.wrappers.lib.wrapPackage {
      package = pkgs.bash;
      inherit pkgs;
      flags."--rcfile" = "${bashConfig}";
    };
  };
}
