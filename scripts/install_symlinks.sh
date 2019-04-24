DOTFILES_ROOT="$( cd "$( dirname "$0" )/.." && pwd )"
now=$(date +"%Y-%m-%d.%s")
arg=$1

backup_file() {
  if [[ $arg != '--force' ]]; then
    if [ -f $1 ] || [ -h $1 ] || [ -d $1 ]; then
      echo "Found $1. Backing up to $1.$now";
      mv $1 $1.$now;
    fi
  fi
}

echo "Symlinking dotfiles..."

# git
backup_file $HOME/.gitconfig
ln -sfn $DOTFILES_ROOT/git/gitconfig $HOME/.gitconfig
backup_file $HOME/.gitignore_global
ln -sfn $DOTFILES_ROOT/git/gitignore_global $HOME/.gitignore_global

# tmux
backup_file $HOME/.tmux.conf
ln -sfn $DOTFILES_ROOT/tmux/tmux.conf $HOME/.tmux.conf
if test ! -d $HOME/.tmux/plugins
then
  mkdir -p $HOME/.tmux/plugins
fi
backup_file $HOME/.tmux/plugins
ln -sfn $DOTFILES_ROOT/tmux/tpm $HOME/.tmux/plugins

# vim
backup_file $HOME/.vim
ln -sfn $DOTFILES_ROOT/vim $HOME/.vim

# neovim
if test ! -d $HOME/.config
then
  mkdir -p $HOME/.config
fi
backup_file $HOME/.config/nvim
ln -sfn $DOTFILES_ROOT/vim $HOME/.config/nvim

# slate
backup_file $HOME/.slate
ln -sfn $DOTFILES_ROOT/slate/slate $HOME/.slate

# emacs
backup_file $HOME/.emacs.d
ln -sfn $DOTFILES_ROOT/emacs.d $HOME/.emacs.d

# zsh
backup_file $HOME/.zsh
ln -sfn $DOTFILES_ROOT/zsh $HOME/.zsh
backup_file $HOME/.zshrc
ln -sfn $DOTFILES_ROOT/zsh/zshrc $HOME/.zshrc
backup_file $HOME/.zshenv
ln -sfn $DOTFILES_ROOT/zsh/zshenv $HOME/.zshenv

# iterm2
backup_file $HOME/.iterm2
ln -sfn $DOTFILES_ROOT/iterm2 $HOME/.iterm2

# ctags
backup_file $HOME/.ctags
ln -sfn $DOTFILES_ROOT/ctags/ctags $HOME/.ctags

# irb
if test ! -d $HOME/.irb
then
  mkdir $HOME/.irb
fi
backup_file $HOME/.irbrc
ln -sfn $DOTFILES_ROOT/irb/irbrc $HOME/.irbrc

# vscode
# eval is need because of the space in the directory path...
VSCODE_ROOT="$HOME/Library/Application\ Support/Code/User"
eval echo $VSCODE_ROOT
# TODO: get this working
# backup_file $VSCODE_ROOT/settings.json
eval ln -sfn $DOTFILES_ROOT/vscode/settings.json $VSCODE_ROOT/settings.json
