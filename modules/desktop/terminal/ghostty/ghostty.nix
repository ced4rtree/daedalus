{ config, ... }: {
  flake.modules.homeManager.ghostty = {
    imports = [ config.flake.modules.homeManager.terminalOption ];

    daedalus.terminalCommand = "ghostty";

    programs.ghostty = {
      enable = true;
      settings = {
        gtk-single-instance = true;
        custom-shader = [
          "${./cursor_blaze.glsl}"
        ];
      };
    };
  };
}
