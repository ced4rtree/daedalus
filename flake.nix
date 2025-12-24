# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    cebar-xyz = {
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs";
      };
      url = "git+https://github.com/ced4rtree/cebar.xyz";
    };
    dark-text = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:vimjoyer/dark-text";
    };
    emacs-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/emacs-overlay";
    };
    firefox-addons = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    };
    flake-file.url = "github:vic/flake-file";
    flake-parts = {
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
      url = "github:hercules-ci/flake-parts";
    };
    hjem = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:feel-co/hjem";
    };
    import-tree.url = "github:vic/import-tree";
    nix-auto-follow = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:fzakaria/nix-auto-follow";
    };
    nix-doom-emacs-unstraightened = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:marienz/nix-doom-emacs-unstraightened";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-lib.follows = "nixpkgs";
    noctalia-shell = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:noctalia-dev/noctalia-shell";
    };
    nvf = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:ced4rtree/nvf/feature/transparent-base16";
    };
    silent-sddm = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:uiriansan/SilentSDDM";
    };
    sops-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:Mic92/sops-nix";
    };
    stylix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:danth/stylix";
    };
    systems.url = "github:nix-systems/default";
    wrappers = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:Lassulus/wrappers";
    };
  };

}
