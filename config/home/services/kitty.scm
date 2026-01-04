(define-module (config home services kitty)
  #:use-module (config util fonts)
  #:use-module (config util colors)
  #:use-module (gnu home services)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (my/home-kitty-service))

(define kitty-config
  (string-append "
font_family " (font-name my/monospace-font) "
bold_font auto
italic_font auto
bold_italic_font auto

cursor_trail 1

# bell configuration
enable_audio_bell no
visual_bell_duration 0
visual_bell_color red

# Cursor customization
cursor_stop_blinking_after 0

# Keybindings
map kitty_mod+f5 load_config_file

# Transparency
background_opacity 1.0

# the rest of this is taken from
# https://github.com/tinted-theming/tinted-kitty

# The basic colors
background " (assoc-ref base16-colors 'base00) "
foreground " (assoc-ref base16-colors 'base05) "
selection_background " (assoc-ref base16-colors 'base03) "
selection_foreground " (assoc-ref base16-colors 'base05) "

# Cursor colors
cursor " (assoc-ref base16-colors 'base05) "
cursor_text_color " (assoc-ref base16-colors 'base00) "

# URL underline color when hovering with mouse
url_color " (assoc-ref base16-colors 'base04) "

# Kitty window border colors
active_border_color " (assoc-ref base16-colors 'base03) "
inactive_border_color " (assoc-ref base16-colors 'base01) "

# OS Window titlebar colors
wayland_titlebar_color " (assoc-ref base16-colors 'base00) "
macos_titlebar_color " (assoc-ref base16-colors 'base00) "

# Tab bar colors
active_tab_background " (assoc-ref base16-colors 'base00) "
active_tab_foreground " (assoc-ref base16-colors 'base05) "
inactive_tab_background " (assoc-ref base16-colors 'base01) "
inactive_tab_foreground " (assoc-ref base16-colors 'base04) "
tab_bar_background " (assoc-ref base16-colors 'base01) "

# The 16 terminal colors
# normal
color0 " (assoc-ref base16-colors 'base00) "
color1 " (assoc-ref base16-colors 'base08) "
color2 " (assoc-ref base16-colors 'base0B) "
color3 " (assoc-ref base16-colors 'base0A) "
color4 " (assoc-ref base16-colors 'base0D) "
color5 " (assoc-ref base16-colors 'base0E) "
color6 " (assoc-ref base16-colors 'base0C) "
color7 " (assoc-ref base16-colors 'base05) "

# bright
color8 " (assoc-ref base16-colors 'base02) "
color9 " (assoc-ref base16-colors 'base08) "
color10 " (assoc-ref base16-colors 'base0B) "
color11 " (assoc-ref base16-colors 'base0A) "
color12 " (assoc-ref base16-colors 'base0D) "
color13 " (assoc-ref base16-colors 'base0E) "
color14 " (assoc-ref base16-colors 'base0C) "
color15 " (assoc-ref base16-colors 'base07)))

(define kitty-file
  (run-with-store (open-connection)
                  (text-file* "kitty.conf" kitty-config)))

(define my/home-kitty-service-type
  (service-type
   (name 'home-kitty-config)
   (extensions
    (list
     (service-extension home-profile-service-type
                        (lambda (config)
                          (list kitty)))
     (service-extension home-xdg-configuration-files-service-type
                        (lambda (config)
                          `(("kitty/kitty.conf" ,kitty-file))))))
   (description "Install kitty and associated config")
   (default-value '())))

(define my/home-kitty-service (service my/home-kitty-service-type))
