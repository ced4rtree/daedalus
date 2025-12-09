{
  flake.modules.nixos.blog = { inputs, pkgs, ... }: {
    services.nginx = {
      enable = true;
      virtualHosts."cebar.xyz" = {
        enableACME = true;
        forceSSL = true;
        root = "${inputs.cebar-xyz.packages.${pkgs.stdenv.hostPlatform.system}.default}/";
      };
    };

    networking.firewall.allowedTCPPorts = [ 80 443 ];

    security.acme = {
      # Accept the CA’s terms of service. The default provider is Let’s Encrypt, you can find their ToS at https://letsencrypt.org/repository/. 
      acceptTerms = true;
      # Optional: You can configure the email address used with Let's Encrypt.
      # This way you get renewal reminders (automated by NixOS) as well as expiration emails.
      defaults.email = "cedar@cebar.xyz";
    };
  };
}
