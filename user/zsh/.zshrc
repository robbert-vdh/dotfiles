#
# User configuration sourced by interactive shells
#

bindkey -e

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

alias firefox="firefox-developer-edition"
alias xsel="xsel -b"
alias xpaste="xsel -o"

# Open emacsclient inside terminal (compliment to ~/.dotfiles/bin/em)
alias en="emacsclient -nw -a ''"

# More modern ls replacement
alias ls="exa --group-directories-first"
alias ll="ls --long --binary --git"
alias la="ll -a"
alias lt="la -T"
alias l="ls -a"
unset LS_COLORS

# Copy files
alias rsync-copy="rsync --archive --hard-links --one-file-system  --acls --xattrs --info=progress2 --human-readable"
alias rsync-move="rsync-copy --remove-source-files"
alias rsync-update="rsync-copy --update"
alias rsync-sync="rsync-copy --update --delete"
alias rsync-copy-compress="rsync-copy --compress"
alias rsync-sync-compress="rsync-sync --compress"

# The dotnet CLI is broken right now https://github.com/dotnet/sdk/issues/1916
alias dotnet="TERM=xterm dotnet"
alias dotnet-dev="ASPNETCORE_ENVIRONMENT=Development dotnet"

# Set the wineprefix to /mnt/data/wine/<prefix>
function wineprefix() {
  if [[ -z $1 ]]; then
    unset WINEPREFIX
    echo "Using default prefix."
  else
    prefix="/mnt/data/wine/$1"
    export WINEPREFIX=$prefix
    echo "Prefix set to '$prefix'."
  fi
}

# Use Git's diffing engine instead of GNU diffutils
function diff() {
  if [[ -z $2 ]]; then
    echo 'Missing argument(s)'
    return 1
  fi
  if [[ ! -e $1 ]]; then
    echo "File or directory '\e[3m$1\e[0m' does not exist"
    return 1
  fi
  if [[ ! -e $2 ]]; then
    echo "File or directory '\e[3m$2\e[0m' does not exist"
    return 1
  fi

  # Right now git's diff does not work well with symlinks
  file1="$(realpath $1)"
  file2="$(realpath $2)"

  if (( $+commands[git] )); then
    git diff --color=auto --no-ext-diff --no-index $file1 $file2
  else
    diff --color=auto $file1 $file2
  fi
}

#
# Moves files to a new location (e.g. another physical volume) whilst leaving a
# symbolic link at the old location.
#
function mvln() {
  if [[ -z $2 ]]; then
    echo "\e[1mUsage\e[0m:"
    echo "mvln \e[3m<source>\e[0m \e[3m<destination>\e[0m"
    return 1
  fi
  if [[ ! -e $1 ]]; then
    echo "File or directory '\e[3m$1\e[0m' does not exist"
    return 1
  fi
  if [[ -e $2 ]]; then
    echo "File or directory '\e[3m$2\e[0m' already exists"
    return 1
  fi

  mv $1 $2
  ln -s $(realpath $2) $1
}

function cheat() {
  curl cheat.sh/$1
}

function jitsi() {
  chromium --app=https://meet.jit.si/$1 >/dev/null 2>&1 &
}
