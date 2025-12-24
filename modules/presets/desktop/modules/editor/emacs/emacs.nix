{ config, ... }: {
  flake-file.inputs.emacs-overlay = {
    url = "github:nix-community/emacs-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  flake.modules.nixos.emacsBase = { lib, pkgs, nixpkgs, ... }: {
    hj.packages = with pkgs; [
      mu
      python312Packages.mutagen
    ];

    services.emacs = {
      enable = true;
      defaultEditor = true;
      startWithGraphical = true;
    };
  };

  perSystem = { pkgs, ... }: {
    # base16 theme taken from stylix source code
    packages.emacs-base16-theme = let
      inherit (config.flake.lib.stylix.colors) withHashtag;
    in pkgs.emacsPackages.trivialBuild (
      with withHashtag; {
        pname = "base16-stylix-theme";
        version = "0.1.0";
        src = pkgs.writeText "base16-stylix-theme.el" ''
          (require 'base16-theme)

          (defvar base16-stylix-theme-colors
            '(:base00 "${base00}"
              :base01 "${base01}"
              :base02 "${base02}"
              :base03 "${base03}"
              :base04 "${base04}"
              :base05 "${base05}"
              :base06 "${base06}"
              :base07 "${base07}"
              :base08 "${base08}"
              :base09 "${base09}"
              :base0A "${base0A}"
              :base0B "${base0B}"
              :base0C "${base0C}"
              :base0D "${base0D}"
              :base0E "${base0E}"
              :base0F "${base0F}")
            "All colors for Base16 stylix are defined here.")

          ;; Define the theme
          (deftheme base16-stylix)

          ;; Add all the faces to the theme
          (base16-theme-define 'base16-stylix base16-stylix-theme-colors)

          ;; Mark the theme as provided
          (provide-theme 'base16-stylix)

          ;; Add path to theme to theme-path
          (add-to-list 'custom-theme-load-path
              (file-name-directory
                  (file-truename load-file-name)))

          (provide 'base16-stylix-theme)
        '';
        packageRequires = [ pkgs.emacsPackages.base16-theme ];
      }
    );
  };
}
