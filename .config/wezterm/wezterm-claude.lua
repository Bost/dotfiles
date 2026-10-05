local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- Equivalent to increasing the default font size three times.
config.font_size = 12.0 * 1.1 ^ 3
config.window_background_opacity = 1.0
config.colors = {
  background = '#262624',
  foreground = '#ECEFF4',
}

return config
