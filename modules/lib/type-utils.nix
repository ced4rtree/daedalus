{ lib, ... }: {
  # Will create an enum whose entries are set to the file/directory names of the
  # specified directory. Useful for creating options of programs to use.
  # For example, if you specifiy a directory that has the following contents:
  # 
  # foo.nix
  # bar.nix
  # baz/
  #
  # then the following enum would be spat out:
  # [
  #   "foo"
  #   "bar"
  #   "baz"
  # ]
  flake.lib.createEnumFromDir = { dir, excludedNames ? [ ] }:
    # lib.types.enum
    #   (dir
    #    |> builtins.readDir
    #    |> builtins.attrNames
    #    |> (builtins.filter (name: lib.lists.elem name excludedNames))
    #    |> (map (x: (builtins.replaceStrings [ ".nix" ] [ "" ]) x)));
lib.types.enum
      (dir
       |> builtins.readDir
       |> builtins.attrNames
       |> (builtins.filter (x: x != "option.nix"))
       |> (builtins.filter (name: !(lib.lists.elem name excludedNames)))
       |> (map (x: (builtins.replaceStrings [ ".nix" ] [ "" ]) x)));
}
