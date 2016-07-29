# allow cd with just dir name
setopt AUTO_CD

# History config
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY SHARE_HISTORY # share history across sessions
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS # Drop extraneous whitespace

# Terminal config
export CLICOLOR=1

# vim settings
export EDITOR=vim
export VISUAL=vim
export GIT_EDITOR=vim

# emacs mode in terminal
set -o emacs
