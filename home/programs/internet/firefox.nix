{ config, lib, pkgs, inputs, ... }: {
  options.daedalus.home.programs.internet.firefox.enable = lib.mkEnableOption "firefox";

  config.stylix.targets.firefox.profileNames = lib.mkIf
    (config.daedalus.home.programs.internet.firefox.enable && config.daedalus.home.stylix.enable)
    [
      "default"
    ];

  config.programs.firefox = lib.mkIf config.daedalus.home.programs.internet.firefox.enable (let
    # taken directly from source
    buildFirefoxXpiAddon = lib.makeOverridable (
      {
        stdenv ? pkgs.stdenv,
        fetchurl ? builtins.fetchurl,
        pname,
        version,
        addonId,
        url,
        sha256,
        meta,
        ...
      }:
      stdenv.mkDerivation {
        name = "${pname}-${version}";

        inherit meta;

        src = fetchurl { inherit url sha256; };

        preferLocalBuild = true;
        allowSubstitutes = true;

        passthru = {
          inherit addonId;
        };

        buildCommand = ''
          dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
          mkdir -p "$dst"
          install -v -m644 "$src" "$dst/${addonId}.xpi"
        '';
      }
    );
  in {
    enable = true;
    package = pkgs.firefox;

    policies.DisableTelemetry = true;
    
    profiles.default = {
      isDefault = true;

      search = {
        force = true;
        default = "ddg";
        privateDefault = "ddg";
      };

      extensions.packages = with inputs.firefox-addons.packages.${pkgs.system}; [
        ublock-origin
        privacy-badger
        facebook-container
        (buildFirefoxXpiAddon {
          pname = "authenticator";
          version = "8.0.2";
          addonId = "{15e21950-869e-4e18-8a6e-8334a3d7d4eb}";
          url = "https://addons.mozilla.org/firefox/downloads/file/4353166/auth_helper-8.0.2.xpi";
          sha256 = "sha256:0qiah54923q9a4n3y8kplap7zkq5rq6v18gyjy2lrprpfaa93ayv";
          meta = with lib; {
            homepage = "https://github.com/Authenticator-Extension/Authenticator";
            description = "Authenticator generates two-factor authentication codes in your browser.";
            license = licenses.mit;
            platforms = platforms.all;
            mozPermissions = [
              "activeTab"
              "storage"
              "identity"
              "alarms"
              "scripting"
              "http://*/*"
              "https://*/*"
              "file:///*"
            ];
          };
        })
      ];
    };
  });
}
