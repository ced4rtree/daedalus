{ config, inputs, ... }: {
  perSystem = { pkgs, lib, ... }: {
    packages.fastfetch = let
      fastfetchConf = lib.generators.toJSON { } {
        logo = {
          source = "${./logo.txt}";
          type = "file";
          color = with config.lib.stylix.colors.withHashtag; {
            "1" = red;
            "2" = green;
            "3" = blue;
            "4" = cyan;
            "5" = magenta;
            "6" = brown;
          };
        };
        modules = [
          "title"
          "separator"
          "os"
          "host"
          "kernel"
          "uptime"
          "packages"
          "shell"
          "display"
          "de"
          "wm"
          "terminal"
          "cpu"
          "gpu"
          "memory"
          "disk"
          "break"
          "colors"
        ];
      };
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = pkgs.fastfetch;

      flags."-c" = pkgs.writeText "fastfetch-config.jsonc" fastfetchConf;
    };
  };
}
