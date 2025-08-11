{ config, lib, pkgs, ... }: {
  options.daedalus.home.programs.internet.discord.enable = lib.mkEnableOption "discord";

  config = lib.mkIf config.daedalus.home.programs.internet.discord.enable {
    # TODO: use nixcord instead
    programs.vesktop = {
      enable = true;
      settings = {
        "FakeNitro" = {
          enabled = true;
          enableEmojiBypass = true;
          emojiSize = 48;
          transformEmojis = true;
          enableStickerBypass = true;
          stickerSize = 160;
          transformStickers = true;
          transformCompoundSentence = false;
          enableStreamQualityBypass = true;
          useHyperLinks = true;
          hyperLinkText = "{{NAME}}";
          disableEmbedPermissionCheck = false;
        };

        "YoutubeAdblock" = {
          enabled = true;
        };
      };
    };
  };
}
