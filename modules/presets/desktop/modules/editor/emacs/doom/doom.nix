{ config, inputs, ... }: {
  perSystem = { pkgs, self', system, ... }: {
    packages.tree-sitter-doxygen = pkgs.stdenv.mkDerivation {
      name = "doxygen-grammar";
      src = pkgs.fetchFromGitHub {
        owner = "ced4rtree";
        repo = "tree-sitter-doxygen";
        rev = "master";
        hash = "sha256-bTwpgBZFWQgPSh0DSYztKOMf/uztUR6o4Ct1OTuGWVA=";
      };

      buildPhase = "make";
      installPhase = ''
        mkdir -p $out/lib
        cp libtree-sitter-doxygen.so $out/lib
      '';
    };

    packages.doomEmacs = let
      inherit (inputs.nix-doom-emacs-unstraightened.packages.${system}) emacs-with-doom;
      emacsWithDoom = emacs-with-doom.override;

      doomDir = let
        inherit (config.flake.lib.stylix) opacity fonts;
        theme = ''
          ;; opacity
          (add-to-list 'default-frame-alist '(alpha-background . ${
            toString (builtins.ceil (opacity.terminal * 100))
          }))

          ;; color scheme
          (require 'base16-stylix-theme)
          (setq base16-theme-256-color-source 'colors)
          (load-theme 'base16-stylix t)

          ;; font
          (setq doom-font (font-spec :family "${fonts.monospace.name}" :size 18))
        '';
      in pkgs.stdenv.mkDerivation {
        src = ./.;
        name = "doomDir";
        patchPhase = ''
          echo "${theme}" >> config.el
        '';

        installPhase = ''
          mkdir -p $out
          cp * $out
        '';
      };
    in (emacsWithDoom {
      emacs = pkgs.emacs-pgtk;

      inherit doomDir;
      doomLocalDir = "~/.local/share/doomemacs/";

      profileName = "";

      extraPackages = epkgs: with epkgs; [
        vterm
        mu4e
        tree-sitter
        tree-sitter-langs
        (treesit-grammars.with-grammars (grammars: with grammars; [
          tree-sitter-bash
          tree-sitter-nix
          tree-sitter-rust
          tree-sitter-java
          tree-sitter-cpp
          tree-sitter-c
        ]))
        self'.packages.tree-sitter-doxygen
        self'.packages.emacs-base16-theme
      ];
    });
  };

  flake.modules.homeManager.doomEmacs = { pkgs, nixpkgs, inputs, ... }: let
    inherit (config.flake.packages.${pkgs.stdenv.hostPlatform.system}) doomEmacs;
  in {
    imports = [ config.flake.modules.homeManager.emacsBase ];

    programs.emacs.package = doomEmacs;
    services.emacs.package = doomEmacs;

    home.packages = with pkgs; [
      fd
    ];

    # install my email config, which is stored as an age encrypted secret
    sops.secrets."emails.el" = {
      path = "/home/${config.daedalus.username}/.local/share/doomemacs/emails.el";
      sopsFile = ./emails.el.age;
      format = "binary";
    };
    systemd.user.services.emacs.unitConfig.After = [ "sops-nix.service" ];
  };
}
