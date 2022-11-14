# Dotfiles

These dotfiles are managed using
[GNU Stow](https://www.gnu.org/software/stow/stow.html).

## Installation

Clone and install: (or you may want to just copy over individual files and
directories instead if you don't want to continue using these dotfiles)

```shell
git clone https://github.com/robbert-vdh/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
./install.sh
```

## PipeWire

If you want to use my [PipeWire
configuration](https://github.com/robbert-vdh/dotfiles/tree/master/user/pipewire),
then you may want to be aware of the following:

1. Make sure you're using the very latest version of PipeWire and WirePlumber.
2. This configuration also uses regular realtime scheduling instead of rtkit, so
   make sure your user has realtime privileges.
3. If you're still using the stock PipeWire media session, then you'll need to
   disable that one first and enable WirePlumber:

   ```shell
   systemctl --user disable --now pipewire-media-session
   systemctl --user enable --now wireplumber
   ```

4. Either copy over the files from the directory linked above to your
   `~/.config` directory, or clone this repository to some place in your home
   directory and use the install script to link everything in place:

   ```shell
   git clone https://github.com/robbert-vdh/dotfiles.git ~/.dotfiles-robbert
   cd ~/.dotfiles-robbert
   ./install.sh user/pipewire
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
Emacs](https://github.com/hlissner/doom-emacs). The files in
`usr/emacs/.emacs.d` can simply be copied or symlinked to wherever Doom is
installed.
