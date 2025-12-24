{ config, ... }: {
  flake.modules.nixos.xdg = {
    xdg.mime.enable = true;
    hj.xdg.config.files."user-dirs.conf".text = ''
      XDG_DESKTOP_DIR="/home/${config.daedalus.username}/Desktop"
      XDG_DOCUMENTS_DIR="/home/${config.daedalus.username}/Documents"
      XDG_DOWNLOAD_DIR="/home/${config.daedalus.username}/Downloads"
      XDG_MUSIC_DIR="/home/${config.daedalus.username}/Music"
      XDG_PICTURES_DIR="/home/${config.daedalus.username}/Pictures"
      XDG_PUBLICSHARE_DIR="/home/${config.daedalus.username}/Public"
      XDG_TEMPLATES_DIR="/home/${config.daedalus.username}/Templates"
      XDG_VIDEOS_DIR="/home/${config.daedalus.username}/Videos"
    '';
  };
}
