DOTFILES_ROOT="$( cd "$( dirname "$0" )/.." && pwd )"

echo "Symlinking dotfiles..."

# git
ln -sfn $DOTFILES_ROOT/git/gitconfig $HOME/.gitconfig
ln -sfn $DOTFILES_ROOT/git/gitignore_global $HOME/.gitignore_global

# tmux
ln -sfn $DOTFILES_ROOT/tmux/tmux.conf $HOME/.tmux.conf

# vim
ln -sfn $DOTFILES_ROOT/vim $HOME/.vim

# neovim
mkdir -p $HOME/.config
ln -sfn $DOTFILES_ROOT/vim $HOME/.config/nvim
ln -sfn $DOTFILES_ROOT/vim/vimrc $HOME/.config/nvim/init.vim

# slate
ln -sfn $DOTFILES_ROOT/slate/slate $HOME/.slate

# emacs
ln -sfn $DOTFILES_ROOT/emacs.d $HOME/.emacs.d

# zsh
ln -sfn $DOTFILES_ROOT/zsh $HOME/.zsh
ln -sfn $DOTFILES_ROOT/zsh/zshrc $HOME/.zshrc
ln -sfn $DOTFILES_ROOT/zsh/zshenv $HOME/.zshenv

# iterm2
ln -sfn $DOTFILES_ROOT/iterm2 $HOME/.iterm2

# ctags
ln -sfn $DOTFILES_ROOT/ctags/ctags $HOME/.ctags

# irb
if test ! -d $HOME/.irb
then
  mkdir $HOME/.irb
fi
ln -sfn $DOTFILES_ROOT/irb/irbrc $HOME/.irbrc
