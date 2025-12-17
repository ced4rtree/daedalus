{ inputs, ... }: {
  imports = with inputs.flake-file.flakeModules; [
    dendritic
    nix-auto-follow
  ];

  flake-file.inputs = {
    flake-file.url = "github:vic/flake-file";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
}
