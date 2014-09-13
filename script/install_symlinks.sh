DOTFILES_ROOT="$( cd "$( dirname "$0" )/.." && pwd )"

echo "Symlinking dotfiles..."

# git
ln -sfn $DOTFILES_ROOT/git/gitconfig ~/.gitconfig
ln -sfn $DOTFILES_ROOT/git/gitignore_global ~/.gitignore_global

# tmux
ln -sfn $DOTFILES_ROOT/tmux/tmux.conf ~/.tmux.conf

# vim
ln -sfn $DOTFILES_ROOT/vim ~/.vim

# slate
ln -sfn $DOTFILES_ROOT/slate/slate ~/.slate

# emacs
ln -sfn $DOTFILES_ROOT/emacs/emacs ~/.emacs

# zsh
ln -sfn $DOTFILES_ROOT/zsh/zshrc ~/.zshrc
