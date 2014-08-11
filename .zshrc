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
plugins=(brew chruby git jekyll)
# custom plugins
plugins+=(fzf)

source $ZSH/oh-my-zsh.sh

# ------------------
# User configuration
# ------------------

# allow cd with just dir name
setopt AUTO_CD

# vim settings
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim
# vim mode in terminal
set -o vi
# vimrc editing
alias ve='vim ~/.vimrc'

# zsh profile editing
alias ze='vim ~/.zshrc'
alias zr='source ~/.zshrc'

# Ruby aliases
alias rtest="ruby -I 'lib:test'"
alias tfdl='tail -f log/development.log'
alias tftl='tail -f log/test.log'

# Bundler aliases
alias be='bundle exec'
alias bi='bundle install'
alias bl='bundle list'
alias bo='bundle open'
alias bp='bundle package'
alias bu='bundle update'
# Bundle exec aliases
alias rake='noglob rake'
alias brake='noglob bundle exec rake'
alias brails='bundle exec rails'
alias bjekyll='bundle exec jekyll'

# git aliases
# info
alias g="git status"
alias gd="git diff"
alias gl="git log"
alias glp="git log  --pretty=\"format:%C(yellow)%h%Cblue%d%Creset %s %C(white) %an, %ar%Creset\" --graph"
# branching
alias gco="git checkout"
alias gcob="git checkout -b"
alias gcom="git checkout master"
alias gbr="git branch"
# committing
alias gci="git commit --verbose"
alias gcia="git commit --verbose --amend"
# pulling/fetching
alias gfa="git fetch --all"
alias gpr="git pull --rebase"
alias gprom="git pull --rebase origin master"
# pushing
alias gpo="git push origin"
alias gpom="git push origin master"
# merging
alias gnoff="git merge --no-ff"
# staging
alias gap="git add -p"
alias gai="git add -i"
alias gaa="git add --all --intent-to-add"
# rebasing
alias gri="git rebase -i"

# ps alias
alias see="ps -ef | ag"

# Pow aliases
# Restart pow
alias restart="touch tmp/restart.txt"
alias restart_parent="touch ~/.pow/restart.txt"
# Clean pow cache
alias clean="dscacheutil -flushcache"

source ~/.fzf.zsh
