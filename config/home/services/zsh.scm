(define-module (config home services zsh)
  #:use-module (config packages pokemon-colorscripts)
  #:use-module (gnu home services shells)
  #:use-module (gnu services)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:export (my/home-zsh-service))

(define zsh-config
  (run-with-store (open-connection) (text-file* "zshrc"
   ;; Enable colors and change prompt
   "autoload -U colors && colors \n"

   "setopt PROMPT_SUBST AUTO_CD CDABLE_VARS CHASE_DOTS AUTO_LIST AUTO_MENU COMPLETE_ALIASES INC_APPEND_HISTORY \n"
   "unsetopt BEEP HIST_BEEP CHASE_LINKS \n"
   "stty stop undef \n"

   ;; urxvt likes to have random bad formatting
   "if [ \"$(cat /proc/$PPID/comm)\" = \"urxvt\" ]; then \n"
   "	clear \n"
   "fi \n"

   ;; set the prompt
   "PS1=\"%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b \" \n"

   ;; aliases
   "alias l='ls --color=auto' \n"
   "alias ls='ls --color=auto' \n"
   "alias ll='ls -l' \n"
   "alias la='ls -a' \n"
   "alias lla='ls -la' \n"
   "alias lh='ls -h' \n"
   "alias llh='ls -lh' \n"
   "alias lha='ls -ah' \n"
   "alias lah='ls -ah' \n"
   "alias llha='ls -lha' \n"
   "alias llah='ls -lha' \n"
   "alias lahl='ls -lha' \n"
   "alias lalh='ls -lha' \n"
   "alias lhal='ls -lha' \n"
   "alias lhla='ls -lha' \n"
   "alias grep='grep --color=auto' \n"

   "alias update-home='guix home reconfigure"
   "  --cores=$(nproc)"
   "  -L ~/.dotfiles/"
   "  ~/.dotfiles/config/home/home.scm' \n"

   "alias update-system='sudo guix system reconfigure"
   "  --cores=$(nproc)"
   "  -L ~/.dotfiles/"
   "  ~/.dotfiles/config/systems/system.scm' \n"

   ;; History in cache directory
   "HISTSIZE=10000 \n"
   "SAVEHIST=10000 \n"
   "HISTFILE=${HOME}/.cache/zsh/history \n"

   ;; completion
   "autoload -U compinit \n"
   "zstyle ':completion:*' menu select \n"
   "zmodload zsh/complist \n"
   "compinit \n"
   "_comp_options+=(globdots) \n" ;; Include hidden files

   ;; add autosuggestions
   "source " zsh-autosuggestions "/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh \n"

   ;; random pokemon at shell startup
   pokemon-colorscripts "/bin/pokemon-colorscripts -r | awk 'NR > 1 { print $0 }' \n"

   ;; fancy prompt
   "eval \"$(" starship "/bin/starship init zsh)\" \n"

   ;; Load zsh-syntax-highlighting; should be last
   "source " zsh-syntax-highlighting "/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh \n")))

(define my/home-zsh-service
  (service home-zsh-service-type
           (home-zsh-configuration
            (zshrc (list zsh-config)))))
