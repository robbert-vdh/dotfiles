#!/usr/bin/env bash

# Source https://gist.github.com/davejamesmiller/1965569
ask() {
    # http://djm.me/ask
    local prompt default REPLY

    while true; do

        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo -n "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read REPLY </dev/tty

        # Default?
        if [ -z "$REPLY" ]; then
            REPLY=$default
        fi

        # Check if the reply is valid
        case "$REPLY" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}

# Update Spacemacs layers
git submodule init > /dev/null
git submodule update > /dev/null

if ask "Dry run?" Y; then
  DRY_RUN=true
else
  DRY_RUN=false

  set -e
  set -o pipefail
fi

echo "Installing configuration..."

function stow2() {
  command="stow $1 -t $2 -v"
  if [[ $1 == '/' ]]; then
    command="sudo $command"
  fi

  if $DRY_RUN; then
    command+=" --no"
    eval $command
  else
    if ask "Install configuration for $package?" Y; then
      eval $command
    fi
  fi
}

cd user
for package in $(ls); do
  if [ -d $package ]; then
    stow2 $package $HOME
  fi
done

echo ""
echo "Installing systemm wide configuration..."
echo "NOTE: Misconfiguration could mess up your system"

cd ../system
for package in $(ls); do
  if [ -d $package ]; then
    stow2 $package '/'
  fi
done
