{ config, pkgs, ... }: {
  # Cursor
  home.pointerCursor = {
     gtk.enable = true;
     package = pkgs.nordzy-cursor-theme;
     name = "Nordzy-cursors";
     size = 32;
  };
}
