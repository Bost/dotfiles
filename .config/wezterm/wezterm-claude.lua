local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- Equivalent to increasing the default font size three times.
config.font_size = 12.0 * 1.1 ^ 3
config.window_background_opacity = 1.0
-- Known window size in cells, e.g. for Claude to draw frames of exact width.
-- With 143 columns, Claude's frames fit at 139 columns (found by trial; the
-- usable width varies by a column or two, so 139 leaves a margin). If you
-- change initial_cols, tell Claude the new frame width.
config.initial_cols = 143
-- config.initial_rows = 40
config.colors = {
  background = '#262624',
  foreground = '#ECEFF4',
}

return config
