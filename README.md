# Dotfiles

These dotfiles are managed using Nix and [Home
Manager](https://github.com/nix-community/home-manager). Most config files are
kept as plain text in `modules/<application_name>/`.

## Installation

If you're reading this you'll probably want to take a look at the individual
config files rather than setting up the entire dotfiles repo with Home Manager,
but this can be done as follows:

```shell
git clone https://github.com/robbert-vdh/dotfiles.git ~/.dotfiles
~/.dotfiles/bin/update-dotfiles
```

## Emacs

My Emacs configuration is based on [Doom
Emacs](https://github.com/hlissner/doom-emacs), which needs to be updated
separately from the main Home Manager config. The files in `modules/emacs/doom`
can simply be copied or symlinked to wherever Doom is installed.

## Nix quirks

Nix, flakes, and Home Manager do 90% of the work well and the other 10% gets
really messy. This is a (likely incomplete list) of the workarounds used in this
Home Manager config for future reference:

- The `bin/update-dotfiles` script wraps around `home-manager switch` with a
  couple additional options:
  - Home Manager is told to specifically use this the flake from this repo's
    root so we don't need to symlink the home manager configuration to
    `~/.config/home-manager` first.
  - The flake is run in impure mode so we can pass the path to this repo to the
    flake through the `DOTFILES_DIR` environment variable.
  - `NIX_CONF_DIR` is also overridden to point to the Nix config in this repo.
    This is needed for the `nix` cli to work so the `nix` cli can be enabled.
  - The script pulls changes from this repo and also makes sure that there are
    no untracked files in `modules/`. Those wouldn't be included in the flake
    which can be kind of confusing.
- Some config files, like Emacs', are symlinked to this repo using absolute
  paths, completely bypassing the Nix store. That is needed for them to be
  mutable, but Home Manager+flakes makes doing so incredibly difficult. That's
  why the aforementioned `DOTFILES_DIR` environment variable is needed, and why
  Home Manager needs to be ran in impure mode.
- `modules/pacman/default.nix` defines an activation script that symlinks some
  files to `/etc`. This is of course not something natively supported or
  encouraged by Home Manager, but it works.
