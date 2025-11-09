{ config, ... }: {
  flake.modules.homeManager.files = {
    home.file.".face.icon".source = ./.face.icon;

    sops.secrets.".mbsyncrc" = {
      sopsFile = ./.mbsyncrc.age;
      format = "binary";
      path = "/home/${config.daedalus.username}/.mbsyncrc";
    };
  };
}
