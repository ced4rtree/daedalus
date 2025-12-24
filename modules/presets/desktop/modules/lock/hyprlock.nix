{ config, inputs, ... }: {
  flake.modules.nixos.hyprlock = { pkgs, ... }: {
    hj.packages = [ config.flake.packages.${pkgs.stdenv.hostPlatform.system}.hyprlock ];
  };

  perSystem = { pkgs, ... }: {
    packages.hyprlock = let
      hyprlockConf = with config.flake.lib.stylix.colors; pkgs.writeText "hyprlock.conf" ''
        general {
          disable_loading_bar = true
          grace = 300
          hide_cursor = true
          no_fade_in = false
        }

        background {
          blur_passes = 1
          blur_size = 6
          path = screenshot
        }

        input-field {
          size = 300, 40
          position = 0, 0
          monitor = 
          dots_center = true
          fade_on_empty = false
          outline_thickness = 5
          placeholder_text = Password
          shadow_passes = 2

          # ooh pretty colors
          outer_color = rgb(${blue})
          inner_color = rgb(${base00})
          font_color = rgb(${base05})
          fail_color = rgb(${base08})
          check_color = rgb(${base0A})
        }
      '';
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.hyprlock;

      flags."-c" = "${hyprlockConf}";
    };
  };
}
