{ config, ... }: {
  flake.modules.nixos.files = {
    hj.files.".face.icon".source = ./.face.icon;

    sops.secrets.".mbsyncrc" = {
      sopsFile = ./.mbsyncrc.age;
      format = "binary";
      path = "/home/${config.daedalus.username}/.mbsyncrc";
    };
  };
}
