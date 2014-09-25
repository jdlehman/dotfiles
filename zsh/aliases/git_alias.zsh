# ====
# info
# ====
alias g="git status"
alias gd="git diff"
alias gdw="git diff --word-diff"
alias gl="git log"
# git log pretty
alias glp="git log --pretty=\"format:%C(yellow)%h%Cblue%d%Creset %s %C(white) %an, %ar%Creset\" --graph"
# git unpushed
alias gup="git log --stat origin/master..HEAD"
# git file overview
alias gfo="git log --pretty=oneline --shortstat --abbrev-commit"
# =========
# branching
# =========
alias gco="git checkout"
alias gcob="git checkout -b"
alias gcom="git checkout master"
alias gb="git branch"
alias gbd="git branch -D"
# ==========
# committing
# ==========
alias gci="git commit --verbose"
alias gcia="git commit --verbose --amend"
# ================
# pulling/fetching
# ================
alias gfa="git fetch --all"
alias gpr="git pull --rebase"
alias gprom="git pull --rebase origin master"
alias gp="git pull"
# =======
# pushing
# =======
alias gpo="git push origin"
alias gpom="git push origin master"
# =======
# merging
# =======
# git no fast forward
alias gnoff="git merge --no-ff"
# =======
# staging
# =======
alias gap="git add -p"
alias gai="git add -i"
alias gaa="git add --all --intent-to-add"
# ========
# rebasing
# ========
alias gri="git rebase -i"
# ========
# stashing
# ========
alias gs="git stash -u"
alias gsp="git stash pop"
alias gsd="git stash drop"
alias gsl="git stash list"
alias gssh="git stash show"
alias gss="git stash save -u"
alias gsb="git stash branch"
# ====
# misc
# ====
# git grep
alias gg="git grep --break --heading -n"
