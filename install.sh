#!/usr/bin/env bash
#
# Every directory in ./user/ will be stowed to $HOME and every directory in
# ./system/ will be stowed to /. If there's an executable file named 'install'
# present in a directory, the script will be executed instead.

# Source https://gist.github.com/davejamesmiller/1965569
ask() {
  # http://djm.me/ask
  local prompt default REPLY

  while true; do
    if [ "${2:-}" = 'Y' ]; then
      prompt='Y/n'
      default=Y
    elif [ "${2:-}" = 'N' ]; then
      prompt='y/N'
      default=N
    else
      prompt='y/n'
      default=
    fi

    # Ask the question (not using "read -p" as it uses stderr not stdout)
    echo -n "$1 [$prompt] "

    # Read the answer (use /dev/tty in case stdin is redirected from somewhere
    # else)
    read -r REPLY </dev/tty

    # Default?
    if [ -z "$REPLY" ]; then
      REPLY=$default
    fi

    # Check if the reply is valid
    case $REPLY in
    Y* | y*) return 0 ;;
    N* | n*) return 1 ;;
    esac
  done
}

cd "$(dirname "$0")"

git submodule update --init --recursive >/dev/null

if ask 'Dry run?' Y; then
  DRY_RUN=1
else
  set -e
  set -o pipefail

  if ask 'Install everything?' N; then
    NO_ASK=1
  fi
fi

function stow2() {
  # Execute a script instead of stowing if an installation script is present
  if [[ -x $1/install ]]; then
    command="$1/install"
  else
    command="stow $1 -t $2 -v"
  fi

  if [[ -z "$DRY_RUN" && $2 == '/' ]]; then
    command="sudo $command"
  fi

  if [[ -n "$DRY_RUN" ]]; then
    command+=" --no 2>&1 \
      | grep -v 'WARNING: in simulation mode so not modifying filesystem.'"
    eval "$command"
  else
    if [[ -n "$NO_ASK" ]] || ask "Install configuration for $package?" Y; then
      eval "$command"
    fi
  fi
}

if [[ -z "$DRY_RUN" ]]; then
  echo 'Installing configuration...'
fi

cd user
for package in *; do
  if [ -d "$package" ]; then
    stow2 "$package" "$HOME"
  fi
done

if [[ -z "$DRY_RUN" ]]; then
  echo ''
  echo 'Installing systemm wide configuration...'
  echo 'NOTE: Misconfiguration could mess up your system'
fi

cd ../system
for package in *; do
  if [ -d "$package" ]; then
    stow2 "$package" '/'
  fi
done
