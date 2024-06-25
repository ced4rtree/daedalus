# Enable colors and change prompt:
autoload -U colors && colors

setopt PROMPT_SUBST AUTO_CD CDABLE_VARS CHASE_DOTS AUTO_LIST AUTO_MENU COMPLETE_ALIASES INC_APPEND_HISTORY 
unsetopt BEEP HIST_BEEP CHASE_LINKS
stty stop undef

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
alias lh='ls -h'
alias llh='ls -lh'
alias lha='ls -ah'
alias lah='ls -ah'
alias llha='ls -lha'
alias llah='ls -lha'
alias lahl='ls -lha'
alias lalh='ls -lha'
alias lhal='ls -lha'
alias lhla='ls -lha'
alias grep='grep --color=auto'

# vim mode
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# bind C-w (w for work) to create a new tmux session for a project
bindkey -s '^w' 'tmux-sessionizer\n'

# History in cache directory:
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=${HOME}/.cache/zsh/history

autoload -U compinit
# Basic auto/tab complete:
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

if pokemon-colorscripts 1>/dev/null; then
	pokemon-colorscripts -r | awk 'NR > 1 { print $0 }'
elif colorscript 2>/dev/null; then
	colorscript random
fi

if starship time 1>/dev/null; then
    eval "$(starship init zsh)"
fi

# Load zsh-syntax-highlighting; should be last.
syntax_path=/usr/share/zsh/plugins/zsh-syntax-highlighting/
if ! [ -d "${syntax_path}" ]; then
    syntax_path="/nix/store/$(ls /nix/store/ | grep zsh-syntax-highlighting | awk '$0 !~ """.drv$""" { print $0 }' | tail -n 1)/share/zsh-syntax-highlighting"
fi
if [ -d "${syntax_path}" ]; then
    source  "${syntax_path}/zsh-syntax-highlighting.zsh" 2>/dev/null
fi
