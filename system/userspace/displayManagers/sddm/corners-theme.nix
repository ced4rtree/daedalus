background: {
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  qt5,
  font ? "Noto Sans",
  fontSize ? "9",
}: stdenvNoCC.mkDerivation rec {
  pname = "sddm-corners";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "CMOISDEAD";
    repo = "sddm-theme-corners";
    rev = "main";
    hash = "sha256-CPK3kbc8lroPU8MAeNP8JSStzDCKCvAHhj6yQ1fWKkY=";
  };

  dontWrapQtApps = true;
  dontBuild = true;

  propagatedUserEnvPkgs = with qt5; [
    qtwayland
    qtquick3d
    qtsvg
    qtgraphicaleffects
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/sddm/themes
    cp -r $src/corners $out/share/sddm/themes/corners

    substituteInPlace $out/share/sddm/themes/corners/theme.conf \
      --replace-fail 'FontFamily="Atkinson Hyperlegible"' 'FontFamily="${font}"' \
      --replace-fail 'FontSize=9' 'FontSize=${fontSize}' \
      --replace-fail 'BgSource="backgrounds/glacier.png"' 'BgSource="${background}"'

    runHook postInstall
  '';
}
