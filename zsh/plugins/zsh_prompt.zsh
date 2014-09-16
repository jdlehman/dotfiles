autoload colors && colors
# based on: http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

if (( $+commands[git] ))
then
  git="$commands[git]"
else
  git="/usr/bin/git"
fi

git_branch() {
  echo $($git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})
}

git_dirty() {
  if $(! $git status -s &> /dev/null)
  then
    echo ""
  else
    if [[ $($git status --porcelain) == "" ]]
    then
      echo "on %{$fg_bold[green]%}$(git_prompt_info)%{$reset_color%}"
    else
      echo "on %{$fg_bold[red]%}$(git_prompt_info)%{$reset_color%}"
    fi
  fi
}

git_prompt_info () {
 ref=$($git symbolic-ref HEAD 2>/dev/null) || return
# echo "(%{\e[0;33m%}${ref#refs/heads/}%{\e[0m%})"
 echo "${ref#refs/heads/}"
}

display_ahead_or_behind() {
  if git rev-parse --git-dir > /dev/null 2>/dev/null
    echo "%{$fg_bold[magenta]%}$(ahead_or_behind)%{$reset_color%}"
  then
    echo ""
  fi
}

ahead_or_behind() {
  # http://stackoverflow.com/a/13172299
  # get the tracking-branch name
  tracking_branch=$(git for-each-ref --format='%(upstream:short)' $(git symbolic-ref -q HEAD))
  set -- $(git rev-list --left-right --count $tracking_branch...HEAD)
  behind=$1
  ahead=$2

  if [[ $behind > 0 ]]
  then
    return_str=" $behind behind"
  elif [[ $ahead > 0 ]]
  then
    return_str=" $ahead ahead"
  else
    return_str=""
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
