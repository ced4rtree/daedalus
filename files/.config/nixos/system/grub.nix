{ config, pkgs, ... }: {
  boot.kernelParams = [ "quiet" "splash" ];
  boot.loader = {
    systemd-boot.enable = false;
    efi.canTouchEfiVariables = true;

    grub.enable = true;
    grub.efiSupport = true;
    grub.device = "nodev";

    grub.theme = pkgs.stdenv.mkDerivation {
      name = "CyberRe";
      pname = "CyberRe";
      src = pkgs.fetchFromGitHub {
        owner = "qdwp";
        repo = "CyberRe";
        rev = "master";
        hash = "sha256-FH/KmmANlxfztroBD1rVdA/XxCGxJPT4CeWWmNs+CIg=";
      };
      installPhase = ''
        cp -r ./CyberRe $out/
      '';
    };

    grub.extraEntries = ''
      menuentry "Reboot" {
        reboot
      }
      menuentry "Poweroff" {
        halt
      }
    '';
  };
}
