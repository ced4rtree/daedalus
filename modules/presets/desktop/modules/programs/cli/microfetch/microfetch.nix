{
  perSystem = { pkgs, ... }: {
    packages.microfetch =
      pkgs.microfetch.overrideAttrs (finalAttrs: previousAttrs: {
        patchPhase = ''
          ls
          pwd
          patch -p1 < ${./colors.patch}
        '';
      });
  };
}
