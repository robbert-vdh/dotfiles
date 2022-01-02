#!/usr/bin/env bash
#
# Usage:
# ./install.sh [user/path1 user/path2 system/path3 ...]
#
# If any paths relative to this script are given as command line arguments, then
# those packages will be installed.
#
# Otherwise, every directory in `user/` will be stowed to `$HOME` and every
# directory in `system/` will be stowed to `/. If there's an executable file
# named 'install' present in a directory, the script will be executed instead.

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

stow2() {
  # Execute a script instead of stowing if an installation script is present
  if [[ -x $1/install ]]; then
    command="'$1/install'"
  else
    command="stow '$(basename "$1")' --dir '$(dirname "$1")' --target '$2' -v"
  fi

  if [[ -z $DRY_RUN && $2 == '/' ]]; then
    command="sudo $command"

    if [[ -z $SUDO_WARNING_PRINTED ]]; then
      echo ''
      echo 'Installing systemm wide configuration...'
      echo 'NOTE: Misconfiguration could mess up your system'

      SUDO_WARNING_PRINTED=1
    fi
  fi

  if [[ -n $DRY_RUN ]]; then
    command+=" --no 2>&1 \
      | grep -v 'WARNING: in simulation mode so not modifying filesystem.' || true"
    eval "$command"
  else
    if [[ -n $NO_ASK ]] || ask "Install configuration for $package?" Y; then
      eval "$command"
    fi
  fi
}

cd "$(dirname "$0")"
git submodule update --init --recursive >/dev/null

if ask 'Dry run?' Y; then
  DRY_RUN=1
else
  set -eo pipefail
fi

if [[ $# -gt 0 ]]; then
  packages=("$@")

  NO_ASK=1
else
  packages=(user/* system/*)

  if ask 'Install everything? (alternatively, pass package names as arguments)' N; then
    NO_ASK=1
  fi
fi

if [[ -n $DRY_RUN ]]; then
  echo -e '\nSimulating install...'
else
  echo -e '\nInstalling configuration...'
fi

for package in "${packages[@]}"; do
  echo "- $package"

  if [[ ! -d $package ]]; then
    echo "'$package' is not a directory"
    exit 1
  fi

  case "$package" in
    user/*)
      target="$HOME"
      ;;
    system/*)
      target="/"
      ;;
    *)
      echo "Unknown package location"
      exit 1
      ;;
  esac

  stow2 "$package" "$target"
done
