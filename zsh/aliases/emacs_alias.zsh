alias ed="emacs --daemon"
# emacs terminal connect to daemon
alias et="emacsclient -t"
# emacs gui connect to daemon
alias eg="emacs-gui"

function emacs-gui {
  which osascript > /dev/null 2>&1 && osascript -e 'tell application "Emacs" to activate'
  emacsclient -n -c "$@"
}
