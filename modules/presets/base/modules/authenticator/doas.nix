{
  flake.modules.nixos.doas = { pkgs, ... }: {
    security.doas = {
      enable = true;
      extraRules = [{
        users = ["cedar"];
        keepEnv = true;
        persist = true;
      }];
    };

    environment.systemPackages = [
      pkgs.doas-sudo-shim
    ];
  };
}
