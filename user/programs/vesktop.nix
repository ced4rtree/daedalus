{ config, lib, pkgs, ... }: {
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
}
