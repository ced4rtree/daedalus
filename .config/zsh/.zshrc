#!/usr/bin/env zsh

# Enable colors and change prompt:
autoload -U colors && colors

setopt PROMPT_SUBST AUTO_CD CDABLE_VARS CHASE_DOTS AUTO_LIST AUTO_MENU COMPLETE_ALIASES INC_APPEND_HISTORY 
unsetopt BEEP HIST_BEEP CHASE_LINKS

# urxvt likes to have random bad formatting
if [ "$(cat /proc/$PPID/comm)" = "urxvt" ]; then
	clear
fi

# set the prompt
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "

alias l='ls --color=auto'
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias grep='grep --color=auto'
alias hx='helix'

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=${HOME}/.cache/zsh/history

# .zcompdump in cache directory
compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-"$ZSH_VERSION"
# same with .zsh_history
export HISTFILE="$XDG_STATE_HOME"/zsh/history

# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

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

# Insult me
if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

function zsh_add_file() {
	[ -f "$HOME/.cache/zsh/plugins/$1" ] && source "$HOME/.cache/zsh/plugins/$1"
}

function zsh_add_plugin() {
	PLUGIN_NAME=$(echo $1 | cut -d "/" -f 2)
	if [ -d "$HOME/.cache/zsh/plugins/$PLUGIN_NAME" ]; then
		# For Plugins
		zsh_add_file "$PLUGIN_NAME/$PLUGIN_NAME.plugin" || \
		zsh_add_file "$PLUGIN_NAME/$PLUGIN_NAME.zsh"
	else 
		git clone "https://github.com/$1.git" "$HOME/.cache/zsh/plugins/$PLUGIN_NAME"
	fi
}

# add autosuggestions
zsh_add_plugin "zsh-users/zsh-autosuggestions"

if [ -f /etc/bash.command-not-found ]; then
    . /etc/bash.command-not-found
fi

if [ -r "/usr/local/opt/pokemon-colorscripts" ]; then
	pokemon-colorscripts -r --no-title
elif [ -r "/opt/shell-color-scripts" ]; then
	colorscript random
fi

eval "$(starship init zsh)"

# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
