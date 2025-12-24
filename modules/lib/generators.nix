{ lib, ... }: {
  flake.lib.generators = {
    # copy/pasted directly from home-manager under modules/lib/generators.nix
    # https://github.com/nix-community/home-manager/blob/527ad07e6625302b648ed3b28c34b62a79bd103e/modules/lib/generators.nix
    toHyprlang =
      {
        indentLevel ? 0,
        importantPrefixes ? [ "$" ],
      }: attrs:
    let
      inherit (lib)
        all
        concatMapStringsSep
        concatStrings
        concatStringsSep
        filterAttrs
        foldl
        generators
        hasPrefix
        isAttrs
        isList
        mapAttrsToList
        replicate
      ;

      initialIndent = concatStrings (replicate indentLevel "  ");

      toHyprlang' =
        indent: attrs:
        let
          sections = filterAttrs (n: v: isAttrs v || (isList v && all isAttrs v)) attrs;

          mkSection =
            n: attrs:
            if lib.isList attrs then
              (concatMapStringsSep "\n" (a: mkSection n a) attrs)
            else
              ''
                ${indent}${n} {
                ${toHyprlang' "  ${indent}" attrs}${indent}}
              '';

          mkFields = generators.toKeyValue {
            listsAsDuplicateKeys = true;
            inherit indent;
          };

          allFields = filterAttrs (n: v: !(isAttrs v || (isList v && all isAttrs v))) attrs;

          isImportantField =
            n: _: foldl (acc: prev: if hasPrefix prev n then true else acc) false importantPrefixes;

          importantFields = filterAttrs isImportantField allFields;

          fields = builtins.removeAttrs allFields (mapAttrsToList (n: _: n) importantFields);
        in
          mkFields importantFields
        + concatStringsSep "\n" (mapAttrsToList mkSection sections)
        + mkFields fields;
    in
      toHyprlang' initialIndent attrs;
  };
}
