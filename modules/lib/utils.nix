{ lib, ... }: {
  flake.lib = rec {
    # returns a list of every file/dir in the specified directory. For example,
    # take the following directory:
    #
    # ./foo/:
    #  - bar.nix
    #  - baz.nix
    #  - bat/
    #
    # calling filenamesIn { dir = ./foo; excludedNames = "bar.nix" } returns the
    # following:
    # [
    #   "baz"
    #   "bat"
    # ]
    filenamesIn = { dir, excludedNames ? [ ] }:
      dir
        |> builtins.readDir
        |> builtins.attrNames
        |> (builtins.filter (x: x != "option.nix"))
        |> (builtins.filter (name: !(lib.lists.elem name excludedNames)))
        |> (map (x: (builtins.replaceStrings [ ".nix" ] [ "" ]) x));

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
    createEnumFromDir = { dir, excludedNames ? [ ] }:
      lib.types.enum (filenamesIn { inherit dir excludedNames; });
  };
}
