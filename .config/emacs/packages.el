(setq

 ;; packages
 packages/evil t ;; evil mode. Setting to nil breaks all SPC- keybdings
 packages/doom-modeline t ;; doom emacs modeline
 packages/spaceline nil ;; spacemacs modeline
 packages/tabs t ;; centaur tabs
 packages/dashboard t ;; dashboard
 packages/autocompletion t ;; code autocompletion. think company, lsp
 packages/treemacs t ;; a file viewer like nerdtree for vim
 packages/projectile t ;; a project manager for emacs
 packages/perspectives t ;; workspaces for emacs
 packages/snippets t ;; code snippets, because my hand are too weak

 ;; minibuffer completion frameworks
 ;;; These aren't meant to be compatible with each other
 minibuffer/ivy nil
 minibuffer/vertico t
 minibuffer/helm nil ;; TODO
 minibuffer/ido nil ;; TODO

 ;; language support
 langs/web nil ;; html, js, css
 langs/java t ;; java
 langs/haskell nil ;; haskell
 langs/c t ;; C & C++

 ;; The Emacs Operating System
 emacsOS/run-launcher t ;; a run launcher like dmenu or rofi
 emacsOS/exwm t ;; an emacs window manager
 emacsOS/elfeed nil ;; an rss feed for emacs
 emacsOS/emms t ;; a music player for emacs
 emacsOS/vterm t ;; a fully featured terminal inside of emacs
 emacsOS/calendar t ;; a nice looking calendar
 emacsOS/mail t ;; a mail client inside of emacs
)
