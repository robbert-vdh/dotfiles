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

## PipeWire

If you want to use my [PipeWire
configuration](https://github.com/robbert-vdh/dotfiles/tree/master/modules/pipewire),
then you may want to be aware of the following:

1. Make sure you're using the very latest version of PipeWire and WirePlumber.
2. The two directories in the directory linked above should be copied to your
   `~/.config` directory.
3. This configuration also uses regular realtime scheduling instead of rtkit, so
   make sure your user has realtime privileges.
4. If you're still using the stock PipeWire media session, then you'll need to
   disable that one first and enable WirePlumber:

   ```shell
   systemctl --user disable --now pipewire-media-session
   systemctl --user enable --now wireplumber
   ```

5. Some things you may want to change depending on your needs are:

   - The ALSA period size and the number of periods. **You'll likely need to
     change this setting.** PipeWire can run on very low ALSA period sizes and
     continuesly push small amounts of audio to the audio device while
     processing larger buffers in the graph. This greatly reduces the audio
     roundtrip latency, but the period size achievable with your hardware very
     much depends on your system. This value can be set in
     `~/.config/wireplumber/main.lua.d/90-alsa-config.lua`, and the default is
     64 frames/period with four periods. If you get any crackling, especially
     while also recording audio, then try changing these settings. Make sure to
     restart WirePlumber afterwards with `systemctl --user restart wireplumber`.
   - The sample rate. This is set to 48000 in my config. Change the sample rate
     values in both `~/.config/pipewire/pipewire.conf` and
     `~/.config/wireplumber/main.lua.d/90-alsa-config.lua` if you want to use a
     different sample rate.
   - The quantum size. This is PipeWire's virtual buffer size (which is
     different from the actual hardware period size used by the underlying ALSA
     backend), and it's always set to 512 samples in this configuration for a
     good tradeoff between low latency and headroom. If you want to change this,
     then change the three quantum size settings in
     `~/.config/pipewire/pipewire.conf` or use
     `pw-metadata -n settings 0 clock.force-quantum N` to temporarily set
     the quantum size to `N` samples.

6. Finally, restart all PipeWire services for the configuration to take effect.

   ```shell
   systemctl --user restart pipewire pipewire-pulse wireplumber
   ```

7. (optional, but recommended)  
   Enable the Pro Audio profile for your audio interface. You can do this in
   your desktop environment's volume manager (e.g. in KDE Plasma there's a
   dropdown menu next to the device's volume level) or in a standalone
   PulseAudio volume management GUI like pavucontrol-qt. This will expose all
   input and output channels on the device instead of acting like a normal
   desktop sound card.

8. (optional, but recommended)  
   Configure PipeWire to always replace JACK. This is now done by default on
   Arch based distros with the `pipewire-jack` package and on Fedora with
   `pipewire-jack-audio-connection-kit`. On other distros you may need to [set this up
   yourself](https://gitlab.freedesktop.org/pipewire/pipewire/-/blob/master/INSTALL.md#jack-emulation).

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
