# My Configuration files for Slock, dwm, st, dwmblocks, dmenu, and zsh

### dwm

my dwm config includes a button to open firefox, volume buttons, and brightness buttons.

### st

Not much has changed here, other than the font2 patch to allow emojis and the dracula patch for a little ricing.

Optionally dependent on Noto Color Emoji font pack for emoji support in st

### dmenu

Literally nothing has changed here, just the default dmenu build.

### dwmblocks

Includes battery, internet speed, brightness and sound level (both have live updates on button press), cpu and ram usage, as well as the date.

Optionally dependent on fish (for my brightness checking script), and alsa (for checking/changing sound)

### zsh

This is a copy of Luke Smith's build, with a couple extra things like opacity using a compositor, and calling ufetch.

Optional dependencies include a compositor like xcompmgr or picom for opacity in the terminal, and ufetch (renamed fetch on my system, as well as located in my /bin directory, so you'll need to do that as well for that to work)

## Important

Programs like dwm and dwmblocks call some scripts from another one of my repos called scripts for brightness, sound, and internet speed, and those functions will not be able to perform without those scripts in the proper directory, or you can clone the scripts and change the directory from which they're called.
