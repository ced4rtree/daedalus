{ config, inputs, ... }: {
  flake.modules.nixos.qutebrowser = { pkgs, ... }: {
    hj.packages = [
      pkgs.python3Packages.adblock
      config.flake.packages.${pkgs.stdenv.hostPlatform.system}.qutebrowser
    ];
  };

  perSystem = { pkgs, lib, ... }: {
    packages.qutebrowser = inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.qutebrowser;

      flags."-C" = let
        qutebrowserConf = with config.flake.lib.stylix.colors.withHashtag; builtins.replaceStrings
          [
            "@base00"
            "@base01"
            "@base02"
            "@base03"
            "@base04"
            "@base05"
            "@base06"
            "@base07"
            "@base08"
            "@base09"
            "@base0A"
            "@base0B"
            "@base0C"
            "@base0D"
            "@base0E"
            "@base0F"
          ]
          [
            base00
            base01
            base02
            base03
            base04
            base05
            base06
            base07
            base08
            base09
            base0A
            base0B
            base0C
            base0D
            base0E
            base0F
          ]
          (builtins.readFile ./config.py);
      in "${pkgs.writeText "qutebrowser-config.py" qutebrowserConf}";
    };
  };
}
