# My Configuration files for polybar, dwm, st, dwmblocks, dmenu, zsh, and urxvt

### dwm

My dwm config includes a button to open firefox, volume buttons, and brightness buttons, among others. The keys for a lot of these buttons must be edited by the end user or they will not work. I'll fix that some afternoon when I've got nothing going on.

### dwmblocks

Includes brightness, sound, and battery level (all have live updates on button press), cpu and ram usage, as well as the date.

The brightness, sound, and net functions are dependent on some scripts from my scripts repo, in conjunction with buttons specified in my dwm build

### zsh

This is a copy of Luke Smith's build, with a couple extra things like opacity using a compositor, and auto plugin managing without a plugin manager

Optional dependencies include a compositor like xcompmgr or picom for opacity in the terminal, ufetch (renamed fetch on my system), and p10k (prompt)

### vim

My .vimrc includes support for relative line numbers, window splitting to the right rather than below, search highlighting, shellcheck, vim command autocompletion, and 4 wide tabs.

The init.vim (for use with neovim) includes syntax completion, better highlighting, and all the features contained in the standard .vimrc 

### urxvt

Pretty much just some cool colors, good fonts, etc.

### Polybar

Almost identical to dwmblocks, just nicer looking.

## Important

Programs like dwm and polybar call some scripts from another one of my repos called scripts for brightness and sound, and those functions will not be able to perform without those scripts.
