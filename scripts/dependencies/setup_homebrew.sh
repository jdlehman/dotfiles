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

# tap emacs-mac (emacs with mac osx fixes/opts)
if ! [[ $(brew tap) =~ "emacsmacport" ]]
then
  # tap
  echo "Tapping railwaycat/emacsmacport"
  brew tap railwaycat/emacsmacport
  # install
  echo "Installing emacs-mac"
  brew install emacs-mac 2> /dev/null
  # link mac app to applications folder
  echo "Symlinking emacs-mac to /Applications"
  brew linkapps emacs-mac 2> /dev/null
fi

# install dependencies with homebrew
install_with_brew \
  ag bash-completion \
  fzf git wget curl \
  automake cmake \
  reattach-to-user-namespace \
  leiningen clojurescript \
  ruby-build chruby \
  python python3 \
  node \
  mysql mongodb postgresql \
  emacs vim cask ispell

brew tap neovim/neovim
brew install --HEAD neovim
