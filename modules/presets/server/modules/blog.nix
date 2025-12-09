{
  flake.modules.nixos.blog = { inputs, pkgs, ... }: {
    services.nginx = {
      enable = true;

      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      # Only allow PFS-enabled ciphers with AES256
      sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";

      appendHttpConfig = ''
        # Add HSTS header with preloading to HTTPS requests.
        # Adding this header to HTTP requests is discouraged
        map $scheme $hsts_header {
            https   "max-age=31536000; includeSubdomains; preload";
        }
        add_header Strict-Transport-Security $hsts_header;
  
        # Enable CSP for your services.
        #add_header Content-Security-Policy "script-src 'self'; object-src 'none'; base-uri 'none';" always;
  
        # Minimize information leaked to other domains
        add_header 'Referrer-Policy' 'origin-when-cross-origin';
  
        # Disable embedding as a frame
        add_header X-Frame-Options DENY;
  
        # Prevent injection of code in other mime types (XSS Attacks)
        add_header X-Content-Type-Options nosniff;
  
        # This might create errors
        proxy_cookie_path / "/; secure; HttpOnly; SameSite=strict";
      '';

      virtualHosts = let
        base = locations: {
          inherit locations;

          forceSSL = true;
          enableACME = true;
        };
        proxy = port: base {
          "/".proxyPass = "http://127.0.0.1:" + toString(port) + "/";
        };
      in {
        "cebar.xyz" = proxy 3000 // {
          default = true;
          enableACME = true;
          forceSSL = true;
          root = "${inputs.cebar-xyz.packages.${pkgs.stdenv.hostPlatform.system}.default}/";
        };
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
