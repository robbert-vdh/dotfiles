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
    ["api.alsa.period-size"]   = 20,
    -- This is super weird right now. Normal ALSA applications or JACK2 can
    -- normally do 64/3 frames/periods, but with PipeWire I still get occasional
    -- crackles with 64/128. Need to investigate this further after a couple
    -- PipeWire updates.
    -- ["api.alsa.period-num"]    = 3,
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
