{ lib, pkgs, ... }:

let
  # This is distributed as a .zip file with no top level directory. Flakes and
  # nixpkgs' `fetchzip` don't understand this.
  anime4kShaders = pkgs.stdenv.mkDerivation {
    name = "Anime4K";
    version = "v4.0.1";

    src = pkgs.fetchurl {
      url =
        "https://github.com/bloc97/Anime4K/releases/download/v4.0.1/Anime4K_v4.0.zip ";
      sha256 = "sha256-E5zSgghkV8Wtx5yve3W4uCUJHXHJtUlYwYdF/qYtftc=";
    };

    # Out of the box Nix doesn't handle .zip files with no top-level directory
    unpackPhase = ''
      unzip -d "$out" "$src"
    '';

    nativeBuildInputs = [ pkgs.unzip ];
  };

  anime4kShaderConfigs = {
    lq = {
      "1080p" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_M.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_S.glsl";
      "720p" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_Soft_M.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_S.glsl";
      "480p" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Upscale_Denoise_CNN_x2_M.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_S.glsl";
      "1080p x2" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_M.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl:${anime4kShaders}/Anime4K_Restore_CNN_S.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_S.glsl";
      "720p x2" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_Soft_M.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Restore_CNN_Soft_S.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_S.glsl";
      "480p x2" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Upscale_Denoise_CNN_x2_M.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Restore_CNN_S.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_S.glsl";
    };

    hq = {
      "1080p" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_VL.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_VL.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl";
      "720p" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_Soft_VL.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_VL.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl";
      "480p" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl";
      "1080p x2" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_VL.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_VL.glsl:${anime4kShaders}/Anime4K_Restore_CNN_M.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl";
      "720p x2" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Restore_CNN_Soft_VL.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_VL.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Restore_CNN_Soft_M.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl";
      "480p x2" =
        "${anime4kShaders}/Anime4K_Clamp_Highlights.glsl:${anime4kShaders}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x2.glsl:${anime4kShaders}/Anime4K_AutoDownscalePre_x4.glsl:${anime4kShaders}/Anime4K_Restore_CNN_M.glsl:${anime4kShaders}/Anime4K_Upscale_CNN_x2_M.glsl";
    };
  };

  # The LQ version looks good enough
  # FIXME: Change this and other settings based on the host name
  anime4kShaderConfig = anime4kShaderConfigs.lq;
  defaultShaders = anime4kShaderConfig."1080p";

  # NOTE: These are taken from
  #       <https://github.com/nix-community/home-manager/blob/master/modules/programs/mpv.nix>.
  #
  #       We can't use the module directly because OpenGL and Vulkan stuff in
  #       Nix just doesn't really work outside of NixOS.
  renderOption = option:
    rec {
      int = toString option;
      float = int;
      bool = lib.hm.booleans.yesNo option;
      string = option;
    }.${builtins.typeOf option};
  renderOptionValue = value:
    let
      rendered = renderOption value;
      length = toString (lib.stringLength rendered);
    in "%${length}%${rendered}";
  renderOptions = lib.generators.toKeyValue {
    mkKeyValue =
      lib.generators.mkKeyValueDefault { mkValueString = renderOptionValue; }
      "=";
    listsAsDuplicateKeys = true;
  };

  renderScriptOptions = lib.generators.toKeyValue {
    mkKeyValue =
      lib.generators.mkKeyValueDefault { mkValueString = renderOption; } "=";
    listsAsDuplicateKeys = true;
  };

  renderBindings = bindings:
    lib.concatStringsSep "\n"
    (lib.mapAttrsToList (name: value: "${name} ${value}") bindings);

in {
  xdg.configFile."mpv/script-opts/osc.conf".text = renderScriptOptions {
    # Without these settings the OSD gets tiny when using a small window
    scalefullscreen = 1.5;
    vidscale = false;
  };

  xdg.configFile."mpv/mpv.conf".text = renderOptions {
    # We can't use the Home Manager module's script management, and adding the
    # packages as is doesn't work because mpv will look in the wrong location.
    # scripts = with pkgs.mpvScripts;
    #   "${autocrop}/share/mpv/scripts/autocrop.lua:${uosc}/share/mpv/scripts/uosc.lua";

    # TODO: Add these back in, depending on the host name
    # Performance tweaks, uncomment some of these if there are stutters
    #deband=no
    #scale=spline36
    #cscale=spline36

    # Window geometry settings
    autofit-larger = "60%x60%";
    geometry = "50%:50%";
    save-position-on-quit = true;
    x11-bypass-compositor = "no";

    # mpv doesn't correctly detect my setup as stereo, causing downmixing problems
    # with 5.1 audio playback
    audio-channels = "stereo";

    # Don't skip images as fast and fix playlist-prev for them
    image-display-duration = "10";
    mf-fps = "0.5";

    # Put that GPU to good use
    hwdec = "auto";
    vo = "gpu-next";
    profile = "gpu-hq";
    gpu-api = "vulkan";

    # Upscaling and motion interpolation
    scale = "ewa_lanczos";
    cscale = "ewa_lanczos";
    # `ewa_lanczossharp` has been deprecated, this restores the old behavior
    scale-blur = 0.981251;
    cscale-blur = 0.981251;

    # From Anime4K, see below for key bindings for switching between different shader configs
    glsl-shaders = defaultShaders;

    # The tempoeral interpolation settings are from
    # https://github.com/mpv-player/mpv/issues/2685#issuecomment-434665993
    interpolation = true;
    tscale = "box";
    tscale-window = "sphinx";
    tscale-clamp = "0.0";
    video-sync = "display-resample";

    # Language and subtitles
    alang = "jp,jpn,en,eng,nl";
    slang = "en,eng,jpn,nl";
    sub-blur = "1.0";
    sub-border-size = "2";
    sub-font = "Roboto";
    sub-font-size = "50";
    sub-margin-y = "40";
    sub-auto = "fuzzy";
    sub-file-paths = "ass:srt:sub:subs:subtitles:ENG";
    demuxer-mkv-subtitle-preroll = "yes";
    volume-max = "200";

    # OSD settings, see ./lua-settings/osc.conf for more
    osd-font = "Source Sans Pro";
    osd-color = "#ffffffff";
    osd-border-color = "#ff151515";
    osd-border-size = "2";
    osd-shadow-offset = "1";
    osd-shadow-color = "#11000000";
    osd-on-seek = "msg-bar";

    # Slightly increase caches for smoother scrubbing
    # Also always enable cache, even for local files. This makes exact scrubbing and
    # backwards scrubbing much more responsive.
    cache = "yes";
    demuxer-max-back-bytes = "128MiB";
    demuxer-max-bytes = "512MiB";
  };

  xdg.configFile."mpv/input.conf".text = renderBindings {
    # The 'strip' subtitle scaling mode is particularly useful when dealing with bad
    # typesetting
    u = ''cycle-values sub-ass-override "strip" "no" "scale"'';

    # These correspond to the default sub-position key bindings
    R = "add sub-scale +0.1";
    T = "add sub-scale -0.1";

    # Adjust subtitle delay to match the previous or next sub
    X = "sub-step -1";
    Z = "sub-step +1";

    # This was bound to T by default
    F = "cycle ontop";

    # Same as CTRL+Left/Right
    "Ctrl+WHEEL_DOWN" = "no-osd sub-seek -1";
    "Ctrl+WHEEL_UP" = "no-osd sub-seek  1";

    "Ctrl+r" = "cycle-values video-rotate 0 90 180 270";
    "Ctrl+a" =
      "playlist-shuffle; playlist-next; playlist-unshuffle; show_text \${playlist}";

    ENTER = "cycle fullscreen";

    # Shaders:
    "CTRL+1" = ''
      no-osd change-list glsl-shaders set "${
        anime4kShaderConfig."1080p"
      }"; show-text "Anime4K: Mode A (1080p)"
    '';
    "CTRL+2" = ''
      no-osd change-list glsl-shaders set "${
        anime4kShaderConfig."720p"
      }"; show-text "Anime4K: Mode B (720p)"
    '';
    "CTRL+3" = ''
      no-osd change-list glsl-shaders set "${
        anime4kShaderConfig."480p"
      }"; show-text "Anime4K: Mode C (480p)"
    '';
    "CTRL+4" = ''
      no-osd change-list glsl-shaders set "${
        anime4kShaderConfig."1080p x2"
      }"; show-text "Anime4K: Mode A+A (1080p x2)"
    '';
    "CTRL+5" = ''
      no-osd change-list glsl-shaders set "${
        anime4kShaderConfig."720p x2"
      }"; show-text "Anime4K: Mode B+B (720p x2)"
    '';
    "CTRL+6" = ''
      no-osd change-list glsl-shaders set "${
        anime4kShaderConfig."480p x2"
      }"; show-text "Anime4K: Mode C+A (480p x2)"
    '';

    "CTRL+0" = ''
      no-osd change-list glsl-shaders clr ""; show-text "GLSL shaders cleared"'';
  };

  # The Anime4K shader paths are directly spliced into the
  # `anime4kShaderConfigs`, so we don't need to set up the directory here
}
