#!/usr/bin/env bash

# Show fetch at top of screen
fetch

# Luke's config for the Zoomer Shell

# Enable colors and change prompt:
autoload -U colors && colors

setopt PROMPT_SUBST AUTO_CD CDABLE_VARS CHASE_DOTS AUTO_LIST AUTO_MENU COMPLETE_ALIASES INC_APPEND_HISTORY 
unsetopt BEEP HIST_BEEP CHASE_LINKS
zle_highlight=('paste:none')

alias kee='keepassxc-cli'
alias ls='ls --color=auto'
alias music='mpv --ao=alsa --shuffle --loop-playlist ${HOME}/music/*.*'
alias wp='swaybg -i wallpapers/$(ls wallpapers | shuf | head -n 1) -m stretch & disown'
alias grep='grep --color=auto'
alias wifi='doas rfkill unblock wifi && iwctl station wlan0 scan'
alias n='nvim'

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=${HOME}/.cache/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' 'lfcd\n'

# Bind ctrl-e to emacs + kill the terminal
bindkey -s '^e' 'emacsclient -c & disown && exit\n'

# Insult me
if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

function zsh_add_file() {
	[ -f "$ZDOTDIR/$1" ] && source "$ZDOTDIR/$1"
}

function zsh_add_plugin() {
	PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
	if [ -d "$ZDOTDIR/plugins/$PLUGIN_NAME" ]; then
		# For Plugins
		zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.plugin" || \
		zsh_add_file "plugins/$PLUGIN_NAME/$PLUGIN_NAME.zsh"
	else 
		git clone "https://github.com/$1.git" "$ZDOTDIR/plugins/$PLUGIN_NAME"
	fi
}

zsh_add_plugin "zsh-users/zsh-autosuggestions"

# Cool prompt
source <(/usr/bin/starship init zsh --print-full-init)

# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
