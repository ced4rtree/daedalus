{ background, colors } : {
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
    hash = "sha256-Ip13yzQWgiaBhoFk3CXgx6psD78/4X/vpNO0RNIRQYw=";
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
      --replace-fail 'BgSource="backgrounds/babilon.png"' 'BgSource="${background}"' \
      --replace-fail '#d4a017' '${colors.blue}' \
      --replace-fail '#D4A017' '${colors.blue}' \
      --replace-fail '#d4af37' '${colors.blue}'

    runHook postInstall
  '';
}
