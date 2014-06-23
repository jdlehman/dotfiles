# -----------------------
# oh-my-zsh configuration
# -----------------------

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
ZSH_THEME="simple"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(bower brew bundler chruby git jekyll)

source $ZSH/oh-my-zsh.sh

# ------------------
# User configuration
# ------------------

export EDITOR='vim'

# enable vi editing mode
set -o vi

# Ruby aliases
alias rtest="ruby -I 'lib:test'"

# git aliases
alias g="git status"

# ps alias
alias see="ps -ef | ag"

# Pow aliases
# Restart pow
alias restart="touch tmp/restart.txt"
alias restart_parent="touch ~/.pow/restart.txt"
# Clean pow cache
alias clean="dscacheutil -flushcache"

# fzf fuzzy search
source ~/.fzf.zsh

export FZF_DEFAULT_OPTS='-x -m'
# Setting ag as the default source for fzf
# export FZF_DEFAULT_COMMAND='ag -l -g ""'
