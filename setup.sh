#!/usr/bin/env bash
# Sets up all the configuration files in their proper spots

# Defaults
auth="doas"

# Sets up some flags
while getopts 'sh' option; do
	case "$option" in
	h)
		echo "This script will move some things around into their correct places and build the suckless utilities for you."
		echo -e "\nFiles in home:\n\t.vimrc\n\t.zshrc\n\t.Xresources (urxvt config)\nFiles in .config:\n\tSuckless utilities\n\tinit.vim (neovim config)"
		echo -e "\nFlags:\n\t -s - Use if you want to use sudo instead of doas"
		echo -e "\n"
		;;
	s)
		auth="sudo"


# Setting up some directories to place things in
mkdir "${HOME}"/.config
mkdir "${HOME}"/.config/suckless
mkdir "${HOME}"/.config/nvim

cp -r {blocks,dwm,dmenu,slock} "${HOME}"/.config/suckless
cp init.vim "${HOME}"/.config/nvim
cp {.zshrc,.vimrc,.Xresources} "${HOME}"

# Building suckless utilities
cd "${HOME}"/.config/suckless/dwm
make
"${auth}" make clean install
cd ../dmenu
make
"${auth}" make clean install
cd ../blocks
make
"${auth}" make clean install
cd ../slock
make
"${auth}" make clean install

# Applying urxvt config
xrdb "${HOME}"/.Xresources

echo "All files/directories have been moved to their proper spot. You may now delete this directory if you would like."
echo -e "\nIf you do not have zsh, vim\neovim, or urxvt installed you may now do so, and all changes should be applied."
