{ inputs, ... }: {
  imports = with inputs.flake-file.flakeModules; [
    dendritic
    # allfollow
  ];

  flake-file.inputs = {
    flake-file.url = "github:vic/flake-file";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
  };
}
