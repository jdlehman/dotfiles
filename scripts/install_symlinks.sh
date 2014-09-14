DOTFILES_ROOT="$( cd "$( dirname "$0" )/.." && pwd )"

echo "Symlinking dotfiles..."

# git
ln -sfn $DOTFILES_ROOT/git/gitconfig $HOME/.gitconfig
ln -sfn $DOTFILES_ROOT/git/gitignore_global $HOME/.gitignore_global

# tmux
ln -sfn $DOTFILES_ROOT/tmux/tmux.conf $HOME/.tmux.conf

# vim
ln -sfn $DOTFILES_ROOT/vim $HOME/.vim

# slate
ln -sfn $DOTFILES_ROOT/slate/slate $HOME/.slate

# emacs
ln -sfn $DOTFILES_ROOT/emacs.d $HOME/.emacs.d

# zsh
ln -sfn $DOTFILES_ROOT/zsh $HOME/.zsh
ln -sfn $DOTFILES_ROOT/zsh/zshrc $HOME/.zshrc
ln -sfn $DOTFILES_ROOT/zsh/zshenv $HOME/.zshenv

# iterm2
ln -sfn $DOTFILES_ROOT/iterm2/com.googlecode.iterm2.plist $HOME/Library/Preferences
