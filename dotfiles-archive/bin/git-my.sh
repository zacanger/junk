#!/usr/bin/env bash

# git-my v1.1.2
#
# Lists a user's remote branches and shows if it was merged
# and/or available locally.
#
# Copyright (c) 2017 David O'Trakoun <me@davidosomething.com>
#
# Requires:
#   bash 4+ for readarray
#
# Usage:
#   git my
#

# is_repository
#
# Exits with error if not a git repository
#
is_repository() {
  git rev-parse --git-dir >/dev/null 2>&1
}

# get_local_branches
#
# Proper way to get a porcelain list of local branches for shell script use
#
get_local_branches() {
  local fmt
  local cmd_get_local_branches

  fmt="
    r=%(refname)
    refname_without_prefix=\${r#refs/heads/}
    printf \"%s\\t%s\\n\" \"\$refname_without_prefix\"
  "

  cmd_get_local_branches=$(
    git for-each-ref --shell --format="$fmt" refs/heads
  )

  eval "$cmd_get_local_branches"
}

# get_everyones_remotes
#
# Get porcelain list of all remote branches
#
# @TODO support remote_name (currently origin)
#
get_everyones_remotes() {
  local fmt
  local cmd_everyones_remotes

  local remote_name
  remote_name=${1:-"origin"}

  # user.name<TAB>branch.name
  # the TAB is used as a delimiter to awk with
  fmt="
    a=%(authorname)
    r=%(refname)
    refname_without_prefix=\${r#refs/remotes/\${remote_name}/}
    printf \"%s\\t%s\\n\" \"\$a\" \"\$refname_without_prefix\"
  "

  cmd_everyones_remotes=$(
    git for-each-ref --shell --format="$fmt" "refs/remotes/$remote_name"
  )

  eval "$cmd_everyones_remotes"
}

# get_merged_remote_branches
#
# @param string optional remote to list merged branches of. Defaults to
#        "origin/master"
# @output names of remote branches that are merged into given branch
get_merged_remote_branches() {
  local remote
  local remote_name
  #local remote_refname
  local merged_remote_branches
  local stripped_branchnames

  remote=${1:-"origin/master"}
  # trim from end of string until / for the remote name
  remote_name=${remote%/*}
  # trim from beginning of string until / for the remote refname
  #remote_refname=${remote#*/}

  merged_remote_branches=$( \
    git branch --no-color \
      --remotes \
      --merged "$remote" \
    )

  # remove "origin/"
  stripped_branchnames=$( \
    echo "$merged_remote_branches" | sed "s/^\s*$remote_name\///g" \
    )

  echo "$stripped_branchnames"
}

# filter_mine
#
# @param git_user
# @param branchnames
# @output branchnames owned by current git user
filter_mine() {
  local git_user
  local branchnames
  git_user=$1
  branchnames=$2

  # use eval to not parse the fmt string
  local my_remotes
  my_remotes=$(echo "$branchnames" | grep -i "^$git_user")

  # output only the branchname
  echo "$my_remotes" | grep -v "HEAD" | awk -F'\t' '{ print $2 }'
}

# merge_lists
#
# Convert two lists to bash arrays, merge them, sort them
#
# @param list1
# @param list2
merge_lists() {
  local l1
  local l2
  l1=$(echo "$1" | awk '{$1=$1};1')
  l2=$(echo "$2" | awk '{$1=$1};1')

  readarray -t a1 <<< "$l1"
  readarray -t a2 <<< "$l2"

  local merged
  merged=( "${a1[@]}" "${a2[@]}" )

  local intersect
  readarray -t intersect < <(printf '%s\n' "${merged[@]}" | sort -u)
  echo "${intersect[@]}"
}

# decorate_merged
#
# @param string my_branches list of remote branch names owned by me
# @param string local_branches list of local branch names
# @param string merged_remote_branches list of all remote branch names merged
#        into another branch
# @output table of branch names and status
decorate_merged() {
  local my_branches
  readarray -t my_branches <<< "$(echo "$1" | tr ' ' '\n' )"

  local local_branches=$2
  local merged_remote_branches=$3

  local normal
  local magenta
  normal=$(tput sgr0)
  magenta=$(tput setaf 5)

  local clreol
  clreol=$'\x1B[K'

  # heading
  echo 'local | tracked | merged'
  echo '----- + ------- + ------'

  # body
  local zebra=0
  local last=$((${#my_branches[@]} - 1))
  for i in "${!my_branches[@]}"; do
    local branchname="${my_branches[$i]}"
    local decorated=''
    local is_local=' '
    local is_tracked=' '
    local is_merged=' '

    echo "$local_branches" | grep -q "$branchname" && \
      is_local='✓'

    if git rev-parse --symbolic-full-name "$branchname@{u}" >/dev/null 2>&1; then
      is_tracked='✓'
    fi

    echo "$merged_remote_branches" | grep -q "$branchname" && \
      is_merged='✓'

    if [ "$branchname" == "$(git symbolic-ref --short HEAD)" ]; then
      branchname="${magenta}${branchname}"
    else
      branchname="${branchname}"
    fi

    decorated=$(printf "   %s  |      %s  |     %s    %s" \
       "$is_local" "$is_tracked" "$is_merged" "$branchname")

    if [ $zebra = 0 ]; then
      echo -en "${normal}"
      printf "\e[48;5;0m%s %s\n" "$decorated" "$clreol"
      zebra=1
    else
      echo -en "${normal}"
      printf "\e[48;5;236m%s %s" "$decorated" "$clreol"
      if [[ "$i" != "$last" ]]; then printf "\n"; fi
      zebra=0
    fi
  done

  printf "\e[48;5;0m\n"
}

main() {
  local decorated
  local everyones_remotes
  local git_remote
  local git_user
  local local_branches
  local merged_remote_branches
  local my_branches
  local my_remotes

  local remote=${1:-"origin/master"}

  # trim from end of string until / for the remote name
  local remote_name=${remote%/*}
  # trim from beginning of string until / for the remote refname
  local remote_ref=${remote#*/}

  git_user=$(git config --get user.name)
  if [ -z "$git_user" ]; then
    echo "ERROR: user.name is not set in git config"
    exit 1
  fi

  git_remote=$(git config --get "remote.${remote_name}.url")
  if [ -z "$git_remote" ]; then
    echo "ERROR: remote.${remote_name}.url is not set"
    exit 1
  fi

  everyones_remotes=$(get_everyones_remotes "$remote_name")
  my_remotes=$(filter_mine "$git_user" "$everyones_remotes")
  merged_remote_branches=$(get_merged_remote_branches "$remote_name/$remote_ref")
  local_branches=$(get_local_branches)
  my_branches=$(merge_lists "$my_remotes" "$local_branches")

  echo
  echo "branches owned by user:  $git_user"
  echo "compare local/merge to:  $remote"
  echo "in remote repository:    $git_remote"
  echo

  decorate_merged "$my_branches" "$local_branches" "$merged_remote_branches"

  echo
}

if ! is_repository; then
  echo "This is not a git repository."
  exit 1
fi

main "$@"
