{ config, inputs, ... }: {
  flake.modules.nixos.gpg = { pkgs, ... }: {
    programs.gnupg = {
      package = config.flake.packages.${pkgs.stdenv.hostPlatform.system}.gnupg;
      agent.enable = true;
      agent.pinentryPackage = pkgs.pinentry-qt;
    };

    hj.xdg.config.files."gnupg/gpg.conf".text = ''
      use-agent
      pinentry-mode loopback
    '';
  };

  perSystem = { pkgs, ... }: {
    packages.gnupg = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.gnupg;

      env.GNUPGHOME = "/home/${config.daedalus.username}/.config/gnupg/";
    };
  };
}
