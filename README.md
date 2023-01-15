# My Configuration files for Slock, dwm, st, dwmblocks, dmenu, and zsh

## Auto configuration

To move everything to a good place that's not wherever you cloned this repository, run

./setup.sh

All files/folders will be placed in ~/.config or your home directory (only hidden files go into home)

### dwm

My dwm config includes a button to open firefox, volume buttons, and brightness buttons. The keys for these buttons must be edited by the end user or they will not work.

### st

Not much has changed here, other than the font2 patch to allow emojis and the dracula patch for a little ricing.

Optionally dependent on Noto Color Emoji font pack for emoji support in st

### dwmblocks

Includes brightness, sound, and battery level (all have live updates on button press), cpu and ram usage, as well as the date.

The brightness, sound, and net functions are dependent on some scripts from my scripts repo, in conjunction with buttons specified in my dwm build

### zsh

This is a copy of Luke Smith's build, with a couple extra things like opacity using a compositor, and calling ufetch.

Optional dependencies include a compositor like xcompmgr or picom for opacity in the terminal, and ufetch (renamed fetch on my system)

### vim

My .vimrc includes support for relative line numbers, window splitting to the right rather than below, search highlighting, shellcheck, vim command autocompletion, and 4 wide tabs.

The init.vim (for use with neovim) includes syntax completion, better highlighting, and all the features contained in the standard .vimrc

## Important

Programs like dwm and dwmblocks call some scripts from another one of my repos called scripts for brightness, sound, and internet speed, and those functions will not be able to perform without those scripts.
