{ lib, pkgs, config, ... }: let
  emacs = pkgs.stdenv.mkDerivation {
    nativeBuildInputs = with pkgs; [ pkg-config autoconf texinfo ];
    buildInputs = with pkgs; [ 
      gtk3
      glib
      librsvg
      libwebp
      gnutls
      tree-sitter
      ncurses
      libgccjit
      emacsPackages.mu4e
    ];
    name = "muh-emacs";
    src = builtins.fetchGit {
      url = "https://git.savannah.gnu.org/git/emacs";
      rev = "3fb966dc6392e1908304a1b6fe481da9f670cfbb";
    };

    configurePhase = ''
      ./autogen.sh
      ./configure --with-pgtk --with-tree-sitter --with-mailutils --prefix=$out
    '';

    buildPhase = "make -j8";

    installPhase = ''
      mkdir -p $out/
      make install
    '';
  };
in {
  home.packages = [ emacs ];
}
