{ config, lib, pkgs, ... }: {
  boot = {
    plymouth = let
      theme = "circle_hud";
      logo = ./nixos-logo.png;
    in {
      enable = true;
      theme = theme;
      themePackages = with pkgs; [
        ((adi1090x-plymouth-themes.override {
          selected_themes = [ theme ];
        }).overrideAttrs (oldAttrs: {
          fixupPhase = ''
            cp ${logo} $out/share/plymouth/themes/${theme}/nixos-logo.png
            echo "
              # display nixos logo
              nixos_image = Image(\"nixos-logo.png\");
              nixos_sprite = Sprite();

              nixos_sprite.SetImage(nixos_image);
              # center the image horizontally
              nixos_sprite.SetX(Window.GetX() + (Window.GetWidth() / 2 - nixos_image.GetWidth() / 2));
              # display just above the bottom of the screen
              nixos_sprite.SetY(Window.GetHeight() - nixos_image.GetHeight() - 50);
            " >> $out/share/plymouth/themes/${theme}/${theme}.script
          '';
        }))
      ];
      logo = logo;
    };

    # silent boot
    consoleLogLevel = 3;
    initrd.verbose = false;
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "udev.log_priority=3"
      "rd.systemd.show_status=auto"
    ];
  };
}
