{ lib, config, pkgs, ... }: {
  options.daedalus.home.editor.emacs.enable = lib.mkEnableOption "emacs";

  config = let
    emacs = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      defaultInitFile = (pkgs.writeText "default.el" ((builtins.readFile ./init.el) + ''
        (add-to-list 'default-frame-alist '(font . "${config.stylix.fonts.monospace.name}-${
          toString (config.stylix.fonts.sizes.terminal * 1.0)
        }"))
        (add-to-list 'default-frame-alist '(alpha-background . ${
          toString (builtins.ceil (config.stylix.opacity.applications * 100))
        }))
        (require 'base16-stylix-theme)
        (setq base16-theme-256-color-source 'colors)
        (load-theme 'base16-stylix t)
      ''));
      alwaysEnsure = true;
      alwaysTangle = true;
      package = pkgs.emacs-unstable-pgtk;
      extraEmacsPackages = epkgs: [
        epkgs.mu4e
        (epkgs.trivialBuild (
          with config.lib.stylix.colors.withHashtag;
          {
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
            packageRequires = [ epkgs.base16-theme ];
          }
        ))
      ];
    };
  in lib.mkIf config.daedalus.home.editor.emacs.enable {
    home.packages = with pkgs; [
      mu
      python312Packages.mutagen
    ];

    services.emacs = {
      enable = true;
      package = emacs;
      defaultEditor = true;
    };

    # I have to add all the stylix stuff manually because the font behaves weird
    stylix.targets.emacs.enable = false;

    programs.emacs = {
      enable = true;
      package = emacs;
    };

    home.file.".config/emacs/early-init.el".source = ./early-init.el;
  };
}
