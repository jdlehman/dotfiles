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
plugins=(brew bundler chruby git jekyll)

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


# -------------
# Opening Files
# -------------

# fe [FUZZY PATTERN] - Open the selected file with the default editor
#   - Bypass fuzzy finder if there's only one match (--select-1)
#   - Exit if there's no match (--exit-0)
fe() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && ${EDITOR:-vim} "$file"
}

# open file
fo() {
  local file
  file=$(fzf --query="$1" --select-1 --exit-0)
  [ -n "$file" ] && open "$file"
}

# --------------------
# Changing Directories
# --------------------

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-*} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}

# cdf - cd into the directory of the selected file
cdf() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# -------
# History
# -------

# fh - repeat history
fh() {
  eval $(([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s | sed 's/ *[0-9]* *//')
}

# ---------
# Processes
# ---------

# fkill - kill process
fkill() {
  ps -ef | sed 1d | fzf -m | awk '{print $2}' | xargs kill -${1:-9}
}

# ---
# Git
# ---

# fbr - checkout git branch
fbr() {
  local branches branch
  branches=$(git branch) &&
  branch=$(echo "$branches" | fzf +s +m) &&
  git checkout $(echo "$branch" | sed "s/.* //")
}

# fco - checkout git commit
fco() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}
