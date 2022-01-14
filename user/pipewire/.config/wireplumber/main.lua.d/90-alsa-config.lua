-- See /usr/share/wireplumber/main.lua.d/50-alsa-config.lua for all properties,
-- this just overrides some settings for low latency USB audio

alsa_monitor.rules[#alsa_monitor.rules + 1] = {
  matches = {
    {
      { "node.name", "matches", "alsa_input.*" },
    },
    {
      { "node.name", "matches", "alsa_output.*" },
    },
  },
  apply_properties = {
    ["audio.rate"]             = 44100,
    ["api.alsa.period-size"]   = 64,
    ["api.alsa.period-num"]    = 4,
    ["api.alsa.disable-batch"] = true,
  },
}

alsa_monitor.rules[#alsa_monitor.rules + 1] = {
  matches = {
    {
      { "node.name", "matches", "alsa_*.usb-*" },
    },
  },
  apply_properties = {
    -- USB audio interfaces can take a while to wake up from suspending
    ["session.suspend-timeout-seconds"] = 0,
  },
}
