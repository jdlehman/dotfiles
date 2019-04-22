# List direcory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'

# zsh profile editing
alias ze='vim ~/.zshrc'
alias zr='source ~/.zshrc'

# ps alias
alias see="ps -ef | grep -i"

# kill process by name
function destroy() { pgrep -f $1 | xargs kill -9}
