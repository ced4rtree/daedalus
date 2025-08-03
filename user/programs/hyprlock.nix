{ config, lib, pkgs, ... }: {
  programs.hyprlock = {
    enable = true;
    settings = {
      general = {
        disable_loading_bar = true;
        grace = 300;
        hide_cursor = true;
        no_fade_in = false;
      };

      background = {
        blur_passes = 1;
        blur_size = 6;
      };

      input-field = {
        size = "300, 40";
        position = "0, 0";
        monitor = "";
        dots_center = true;
        fade_on_empty = false;
        outline_thickness = 5;
        placeholder_text = "Password";
        shadow_passes = 2;
      };
    };
  };
}
