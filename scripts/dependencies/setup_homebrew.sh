# install homebrew
if ! [[ $( which brew ) ]]
then
  echo "Installing homebrew..."
  ruby -e "$( curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install )"
fi

# update brew formulae
brew update

install_with_brew() {
  # install each uninstalled dependency
  installed_packages=$(brew list)
  while [ "$1" ]
  do
    if ! [[ $installed_packages =~ $1 ]]
    then
      echo "Installing $1"
      brew install $1 2> /dev/null
    fi
    shift
  done
}

# install dependencies with homebrew
install_with_brew \
  ag fzf git wget curl \
  reattach-to-user-namespace \
  ruby-install chruby \
  python python3 \
  node tmux go godep \
  mysql postgresql \
  emacs nvim vim ispell \
  opam
