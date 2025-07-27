{ config, pkgs, ... }: {
  boot.kernelParams = [ "quiet" "splash" ];
  boot.loader = {
    systemd-boot.enable = false;
    efi.canTouchEfiVariables = true;

    grub = {
      enable = true;
      efiSupport = true;
      devices = [ "nodev" ];

      theme = pkgs.stdenv.mkDerivation {
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

      extraEntries = ''
        menuentry "Arch Linux" {
          insmod part_gpt
          insmod fat
          search --no-floppy --fs-uuid --set=root CA84-2C54
          linux /vmlinuz-linux-zen root=UUID=f4b92ad9-c69a-4f3d-a305-241bd4b6eb42 rw zswap.enabled=0 rootfstype=ext4 loglevel=3 quiet splash
          initrd /intel-ucode.img /initramfs-linux-zen.img
        }

        menuentry "Reboot" {
          reboot
        }
        menuentry "Poweroff" {
          halt
        }
      '';
    };
  };
}
