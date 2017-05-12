#
# User configuration sourced by interactive shells
#

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

# Set the path
typeset -gU cdpath fpath mailpath path

path=(
  /usr/local/{bin,sbin}
  $HOME/.cargo/bin
  $HOME/.dotfiles/bin
  $HOME/.local/bin
  $path
)

# Configure editors
export EDITOR='nvim'
export VISUAL=$EDITOR
export PAGER='less'
export BROWSER='firefox'

# Emacs' terminal prints the raw color codes
if [ $TERM != 'eterm-color' ]; then
  # Use base16 colors in VIM
  BASE16_SHELL=$HOME/.config/base16-shell/
  [ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] &&
      eval "$($BASE16_SHELL/profile_helper.sh)"
fi

# Fix opening new windows in various terminal emulators
source /etc/profile.d/vte.sh

# Custom aliases

# Launch Emacs in terminal mode
alias et="emacsclient -t -a ''"
# Launch Emacs in regular GUI mode, attaching to a currently open frame
alias ea="emacsclient -n -a ''"
# Launch Emacs in regular GUI mode, creating a new frame
alias ec="emacsclient -nc -a ''"

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
