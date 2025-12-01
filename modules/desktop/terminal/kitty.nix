{
  flake.modules.homeManager.kitty = { config, ... }: {
    programs.kitty = {
      enable = true;
      settings = {
        cursor_trail = 1;
        enable_audio_bell = false;
        visual_bell_duration = 0.5;
        visual_bell_colors = config.lib.stylix.colors.withHashtag.red;
      };
    };
  };
}
