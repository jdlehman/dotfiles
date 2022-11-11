# jdlehman's dotfiles

These are my personal dotfiles. While you are free fork the repo and use the parts that you like, keep in mind that I might not merge any pull requests (though if you think you have better ways of doing things or something I might find useful, by all means give it a go). I currently use MacOSX, so that is the only supported platform. For Linux variants, you should be able to get everything working with minimal modifications. The only thing I can think of is that you will need to install dependencies with something other than [Homebrew](http://brew.sh/).

## Installation

To setup and install these dotfiles, simply run `./install` from the project's directory. This will setup any dependencies you need and symlink the dotfiles in the appropriate places.

## Structure

The following is a high-level overview of the project's structure. You can look into the source for the specifics. Each folder with the exception of `scripts` contains dotfiles to be symlinked into the `$HOME` directory.

## Credits

I drew inspiration as well as structure from[ Holman's](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/) [dotfiles](https://github.com/holman/dotfiles). I highly recommend taking a look at his dotfiles as well, you might prefer the overall structure and design of his setup.
