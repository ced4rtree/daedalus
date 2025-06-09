{ lib, config, pkgs, ... }: {
  services.mako = {
    enable = true;
    settings = ''
default-timeout = 5000 # 5 seconds

background-color = "#303446"
text-color = "#c6d0f5"
border-color = "#8caaee"
progress-color = "over #414559"

"urgency=high" = {
  border-color = "#ef9f76"
}
'';
  };
}
