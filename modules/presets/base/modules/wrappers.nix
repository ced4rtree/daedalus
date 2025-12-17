{
  flake-file.inputs.wrappers = {
    url = "github:Lassulus/wrappers";
    inputs.nixpkgs.follows = "nixpkgs";
  };
}
