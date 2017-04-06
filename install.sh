#!/usr/bin/env bash
set -e

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
fi

echo "Installing configuration..."

cd user
for package in $(ls); do
  if [ -d $package ]; then
    if $DRY_RUN; then
      stow --no $package -t $HOME -v || true
    else
      if ask "Install configuration for $package?" Y; then
        stow $package -t $HOME -v
      fi
    fi
  fi
done

echo ""
echo "Installing systemm wide configuration..."
echo "NOTE: Misconfiguration could mess up your system"

cd ../system
for package in $(ls); do
  if [ -d $package ]; then
    if $DRY_RUN; then
      stow --no $package -t / -v || true
    else
      if ask "Install global configuration for $package?" Y; then
        sudo stow $package -t / -v
      fi
    fi
  fi
done
