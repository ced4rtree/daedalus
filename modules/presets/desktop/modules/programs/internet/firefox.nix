{
  flake-file.inputs.firefox-addons.url = "gitlab:rycee/nur-expressions?dir=pkgs/firefox-addons";

  flake.modules.homeManager.firefox = { config, lib, pkgs, inputs, ... }: {
    stylix.targets.firefox.profileNames = [
      "default"
    ];

    programs.firefox = let
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

        extensions.packages = with inputs.firefox-addons.packages.${pkgs.stdenv.hostPlatform.system}; [
          ublock-origin
          privacy-badger
          facebook-container
          (buildFirefoxXpiAddon {
            pname = "auth-helper";
            version = "8.0.2";
            addonId = "authenticator@mymindstorm";
            url = "https://addons.mozilla.org/firefox/downloads/file/4353166/auth_helper-8.0.2.xpi";
            sha256 = "dbab91947237df4c8597fea1b00dce05cf7faea277223f2c51090f9148812a62";
            meta = with lib;
              {
                homepage = "https://github.com/Authenticator-Extension/Authenticator";
                description = "Authenticator generates 2-Step Verification codes in your browser.";
                license = licenses.mit;
                mozPermissions = [
                  "activeTab"
                  "storage"
                  "identity"
                  "alarms"
                  "scripting"
                ];
                platforms = platforms.all;
              };
          })
        ];
      };
    };
  };
}
