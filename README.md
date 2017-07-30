# jdlehman's dotfiles

These are my personal dotfiles. While you are free fork the repo and use the parts that you like, keep in mind that I might not merge any pull requests (though if you think you have better ways of doing things or something I might find useful, by all means give it a go). I currently use MacOSX, so that is the only supported platform. For Linux variants, you should be able to get everything working with minimal modifications. The only thing I can think of is that you will need to install dependencies with something other than [Homebrew](http://brew.sh/).

## Installation

To setup and install these dotfiles, simply run `sh scripts/installer` from the project's directory. This will setup any dependencies you need and symlink the dotfiles in the appropriate places. Any existing dotfiles will be backed up by date unless the installer is run with `--force`.

## Structure

The following is a high-level overview of the project's structure. You can look into the source for the specifics. Each folder with the exception of `scripts` contains dotfiles to be symlinked into the `$HOME` directory. One thing to note is that each of these folders can contain install shell scripts to setup anything related to that particular dotfile. These additional shell scripts can be in the form `install*.sh` or `setup*.sh`. Setup scripts are run first, and typically contain dependencies that might be needed before other scripting occurs. The install scripts contain whatever else needs to be done to set up these dotfiles.

## Credits

I drew inspiration as well as structure from[ Holman's](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/) [dotfiles](https://github.com/holman/dotfiles). I highly recommend taking a look at his dotfiles as well, you might prefer the overall structure and design of his setup.
