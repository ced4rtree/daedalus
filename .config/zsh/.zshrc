# Show fetch at top of screen
if ! where fetch; then
	fetch
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Enable colors and change prompt:
autoload -U colors && colors

setopt PROMPT_SUBST AUTO_CD CDABLE_VARS CHASE_DOTS AUTO_LIST AUTO_MENU COMPLETE_ALIASES INC_APPEND_HISTORY 
unsetopt BEEP HIST_BEEP CHASE_LINKS
zle_highlight=('paste:none')

alias ls='ls --color=auto'
alias music='mpv --ao=alsa --shuffle --loop-playlist ${HOME}/music/*.*'
alias wp='feh --bg-scale ~/photos/$(ls ~/photos --format=single-column | shuf)'
alias grep='grep --color=auto'
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

# Insult me
. /etc/bash.command-not-found

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

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

if [[ $(pidof Xorg) != "" ]]; then
	if [ -f ~/.p10k.zsh ]; then
		source ~/powerlevel10k/powerlevel10k.zsh-theme
		export brightdisp=$(xrandr | awk 'NR==2 {print $1}')
		
		# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
		[[ ! -f ~/p10k.zsh ]] || source ~/p10k.zsh
		
		# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
		[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
	fi
else
	PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
	PROMPT="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
	RPROMPT=""
fi

# Load zsh-syntax-highlighting; should be last.
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
