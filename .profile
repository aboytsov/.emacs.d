echo
echo "Welcome."
echo

export PATH=~/bin:$PATH

unset PS1
unset PS2

is_git_repository() {
  git branch > /dev/null 2>&1
}

get_git_branch() {
    branch_name=$(git symbolic-ref -q HEAD)
    branch_name=${branch_name##refs/heads/}
    branch_name=${branch_name:-HEAD}
    printf $branch_name
}

set_color() {
    tput setaf $1
}

cprint() {
    set_color $1
    shift
    printf "$@"
}

prompt=" ⌘ "

color_black=0
color_red=1
color_green=2
color_yellow=3
color_blue=4
color_magenta=5
color_cyan=6
color_gray=7

color_non_git_path=$color_blue
color_git_path=$color_blue
color_git_branch=$color_cyan
color_prompt=$color_blue
color_right=$color_gray

prompt_func() {
    relative_pwd=${PWD/~/\~}
    if is_git_repository; then
        git_root=$(git rev-parse --show-toplevel)
        relative_git_root=${git_root/~/\~}
        git_path=${relative_pwd/$relative_git_root/}
        non_git_path=${relative_pwd/$git_path/}
        git_branch=@$(get_git_branch)
    else
        git_path=
        non_git_path=$relative_pwd
        git_branch=
    fi

    cprint $color_non_git_path "%s" "$non_git_path"
    cprint $color_git_branch "%s" "$git_branch"
    cprint $color_git_path "%s" "$git_path"
    cprint $color_prompt "%s" "$prompt"
    tput sc

    right="$(whoami)@$(hostname)"
    num_spaces=$((${COLUMNS}-${#right}-${#non_git_path}-${#git_branch}-\
                            ${#git_path}-${#prompt}))
    cprint $color_gray "%${num_spaces}s" " "

    cprint $color_right $right
    tput rc

    tput sgr0
    tput bold
}

export PROMPT_COMMAND=prompt_func
trap "tput sgr0" DEBUG
