{ config, ... }: {
  flake.modules.homeManager.doomEmacs = { pkgs, ... }: let
    cfg = config.daedalus.emacs;
    tree-sitter-doxygen = pkgs.stdenv.mkDerivation {
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
    emacsPkg = pkgs.emacs.pkgs.overrideScope (self: super: {
      emacs = pkgs.emacs30-pgtk;
    });
    emacs = emacsPkg.withPackages (epkgs: with epkgs; [
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
      tree-sitter-doxygen
    ]);
  in {
    imports = [ config.flake.modules.homeManager.emacsBase ];
    xdg.configFile."emacs".source = pkgs.fetchFromGitHub {
      owner = "doomemacs";
      repo = "doomemacs";
      rev = "master";
      hash = "sha256-qfoBM8/s5cqyJKZykEoLws8J+AZZ25jL7z0MLS0Rdg0=";
    };

    xdg.configFile."doom" = {
      source = ./.;
      # onChange = "${pkgs.writeShellScript "doom-change" ''
      #   #!${pkgs.bash}/bin/bash
      #   export PATH="${pkgs.git}/bin:${emacs}/bin:$PATH"
      #   if [ -z "$DOOMLOCALDIR" ]; then
      #     $HOME/.config/emacs/bin/doom install
      #   else
      #     $HOME/.config/emacs/bin/doom sync
      #   fi
      # ''}";
    };

    programs.emacs.package = emacs;
    services.emacs.package = emacs;

    systemd.user.sessionVariables = {
      DOOMLOCALDIR = "$HOME/.local/share/doomemacs";
      DOOMPROFILELOADFILE = "$HOME/.local/share/doomemacs/profiles/load.el";
    };
  };
}
