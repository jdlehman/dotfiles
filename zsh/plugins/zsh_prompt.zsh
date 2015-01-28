autoload colors && colors

if (( $+commands[git] ))
then
  git="$commands[git]"
else
  git="/usr/bin/git"
fi

git_dirty() {
  if $(! $git status -s &> /dev/null)
  then
    echo ""
  else
    if [[ $($git status --porcelain) == "" ]]
    then
      echo "on %{$fg_bold[green]%}$(git_branch)%{$reset_color%}"
    else
      echo "on %{$fg_bold[red]%}$(git_branch)%{$reset_color%}"
    fi
  fi
}

git_branch () {
  local branch=$($git rev-parse --abbrev-ref HEAD 2>/dev/null)
  if [[ $branch == "HEAD" ]]
  then
    echo $($git status | head -1)
  else
    echo $branch
  fi
}

display_ahead_or_behind() {
  echo "%{$fg_bold[magenta]%}$(ahead_or_behind 2>/dev/null)%{$reset_color%}"
}

ahead_or_behind() {
  # http://stackoverflow.com/a/13172299
  # get the tracking-branch name
  local tracking_branch=$($git for-each-ref --format='%(upstream:short)' $($git symbolic-ref -q HEAD))
  set -- $($git rev-list --left-right --count $tracking_branch...HEAD)
  local behind=$1
  local ahead=$2

  local return_str=""
  if [[ $behind > 0 ]]
  then
    return_str+=" $behind behind"
  fi

  if [[ $ahead > 0 ]]
  then
    return_str+=" $ahead ahead"
  fi

  echo $return_str
}

directory_name() {
  echo "%{$fg_bold[cyan]%}%1/%\/%{$reset_color%}"
}

set_prompt () {
  export PROMPT="in $(directory_name) $(git_dirty)$(display_ahead_or_behind) > "
}

precmd() {
  set_prompt
}
