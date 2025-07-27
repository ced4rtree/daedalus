{ config, lib, pkgs, ... }: {
  home.file.".gnupg/gpg.conf".text = ''
    use-agent
    pinentry-mode loopback
  '';
}
