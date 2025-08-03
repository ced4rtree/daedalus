{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  just,
  kdePackages,
  font ? "Noto Sans",
  fontSize ? "9",
}:
stdenvNoCC.mkDerivation rec {
  pname = "sddm-gruvbox";
  version = "1.1.0";

  src = fetchFromGitHub {
    owner = "he1senbrg";
    repo = "sddm-gruvbox";
    rev = "v${version}";
    hash = "sha256-oM5PxNO0RTeOrWZ19gILnzhE43D4FVs6+n0enrofA6E=";
  };

  dontWrapQtApps = true;

  nativeBuildInputs = [
    just
  ];

  propagatedBuildInputs = [
    kdePackages.qtsvg
  ];

  buildPhase = ''
    runHook preBuild

    just build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/sddm/themes
    cp -r dist/sddm-gruvbox $out/share/sddm/themes/sddm-gruvbox

    substituteInPlace $out/share/sddm/themes/sddm-gruvbox/theme.conf \
      --replace-fail 'Font="Noto Sans"' 'Font="${font}"' \
      --replace-fail 'FontSize=10' 'FontSize=${fontSize}'

    runHook postInstall
  '';

  postFixup = ''
    mkdir -p $out/nix-support
    echo ${kdePackages.qtsvg} >> $out/nix-support/propagated-user-env-packages
  '';
}
