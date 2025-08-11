{ lib, ... }: {
  scanPaths = 
    path:
    builtins.readDir path
      |> lib.attrsets.filterAttrs (
        name: type:
        (type == "directory") || ((name != "default.nix") && (lib.strings.hasSuffix ".nix" name))
      )
      |> builtins.attrNames
      |> builtins.map (f: "${path}/${f}");
}
