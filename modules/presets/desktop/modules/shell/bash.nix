{ config, ... }: let
  topConfig = config;
in {
  flake.modules.nixos.bash = { pkgs, ... }: {
    programs.bash.enable = true;
    environment.shells = [ pkgs.bash ];
    users.users.${topConfig.daedalus.username}.shell = pkgs.bash;
  };

  flake.modules.homeManager.bash = { lib, pkgs, ... }: {
    programs.bash = {
      enable = true;
      initExtra = ''
        function parse_git_branch() {
            BRANCH=`git branch 2>/dev/null | grep '^\*' | colrm 1 2`
            if [ ! $BRANCH == "" ]; then
                echo " $BRANCH"
            else
                echo ""
            fi
        }
        
        export PS1="\[\e[34m\]\w\[\e[32m\]\`parse_git_branch\` \[\e[35m\]->\[\e[0m\] "

        ${lib.getExe topConfig.flake.packages.${pkgs.stdenv.hostPlatform.system}.microfetch}
      '';
    };
  };
}
