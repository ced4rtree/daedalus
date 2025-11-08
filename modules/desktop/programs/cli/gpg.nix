{
  flake.modules.homeManager.gpg = { pkgs, ... }: {
    home.packages = with pkgs; [
      gnupg
      pinentry-qt
    ];

    home.file.".gnupg/gpg.conf".text = ''
      use-agent
      pinentry-mode loopback
    '';
  };
}
