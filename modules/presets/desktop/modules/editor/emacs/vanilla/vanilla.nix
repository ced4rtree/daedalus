{ config, inputs, ... }: {
  flake.modules.homeManager.vanillaEmacs = { pkgs, ... }: let
    inherit (config.flake.packages.${pkgs.stdenv.hostPlatform.system}) vanillaEmacs;
  in {
    imports = [ config.flake.modules.homeManager.emacsBase ];

    programs.emacs.package = vanillaEmacs;
    services.emacs.package = vanillaEmacs;

    # install my email config, which is stored as an age encrypted secret
    sops.secrets."emails.el" = {
      path = "/home/${config.daedalus.username}/.local/share/emacs/emails.el";
      sopsFile = ./emails.el.age;
      format = "binary";
    };
    systemd.user.services.emacs.unitConfig.After = [ "sops-nix.service" ];
  };

  perSystem = { pkgs, self', system, ... }: {
    packages.vanillaEmacs = let
      configDir = let
        inherit (config.flake.lib.stylix) opacity fonts;
        theme = ''
          ;; font
          (add-to-list 'default-frame-alist '(font . \"${fonts.monospace.name}-${toString fonts.sizes.terminal}\"))
  
          ;; opacity
          (add-to-list 'default-frame-alist '(alpha-background . ${
            toString (builtins.ceil (opacity.terminal * 100))
          }))
  
          ;; color scheme
          (require 'base16-stylix-theme)
          (setq base16-theme-256-color-source 'colors)
          (load-theme 'base16-stylix t)
        '';
      in (pkgs.stdenv.mkDerivation {
        src = ./.;
        name = "configDir";
        patchPhase = ''
          echo "${theme}" >> init.el
        '';

        installPhase = ''
          mkdir -p $out
          cp * $out
        '';
      }).outPath;
    in inputs.wrappers.lib.wrapPackage {
      inherit pkgs;
      package = self'.packages.vanillaEmacsUnwrapped;
      flags = {
        "--init-directory" = configDir;
      };
    };

    packages.vanillaEmacsUnwrapped = let
      lib = inputs.emacs-overlay.lib.${pkgs.stdenv.hostPlatform.system};
      inherit (config.flake.lib.stylix) fonts opacity;
    in lib.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      alwaysEnsure = true;
      alwaysTangle = true;
      package = inputs.emacs-overlay.packages.${system}.emacs-unstable-pgtk;
      extraEmacsPackages = epkgs: [
        epkgs.mu4e
        config.flake.packages.${pkgs.stdenv.hostPlatform.system}.emacs-base16-theme
      ];
    };
  };
}
