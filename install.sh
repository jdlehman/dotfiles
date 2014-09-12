#!/bin/sh

DOTFILES_ROOT="$( cd "$( dirname "$0" )" && pwd )"

# git
ln -sfn $DOTFILES_ROOT/gitconfig ~/.gitconfig
ln -sfn $DOTFILES_ROOT/gitignore_global ~/.gitignore_global

# tmux
ln -sfn $DOTFILES_ROOT/tmux.conf ~/.tmux.conf

# vim
ln -sfn $DOTFILES_ROOT/vim ~/.vim

# slate
ln -sfn $DOTFILES_ROOT/slate ~/.slate

# emacs
ln -sfn $DOTFILES_ROOT/emacs ~/.emacs

# zsh
ln -sfn $DOTFILES_ROOT/zshrc ~/.zshrc

echo "Dotfiles symlinked!"
