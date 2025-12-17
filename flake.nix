# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    cebar-xyz.url = "git+https://github.com/ced4rtree/cebar.xyz";
    dark-text.url = "github:vimjoyer/dark-text";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    firefox-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";
    flake-file.url = "github:vic/flake-file";
    flake-parts = {
      inputs.nixpkgs-lib.follows = "nixpkgs-lib";
      url = "github:hercules-ci/flake-parts";
    };
    home-manager.url = "github:nix-community/home-manager";
    import-tree.url = "github:vic/import-tree";
    nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
    nixcord.url = "github:KaylorBen/nixcord";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-lib.follows = "nixpkgs";
    noctalia-shell.url = "github:noctalia-dev/noctalia-shell";
    nvf.url = "github:ced4rtree/nvf/feature/transparent-base16";
    silent-sddm.url = "github:uiriansan/SilentSDDM";
    sops-nix.url = "github:Mic92/sops-nix";
    stylix.url = "github:danth/stylix";
    systems.url = "github:nix-systems/default";
    wrappers.url = "github:Lassulus/wrappers";
  };

}
