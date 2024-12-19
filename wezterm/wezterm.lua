local wezterm = require 'wezterm'

local config = {}
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- setup

-- config.color_scheme = 'Modus-Vivendi'

config.window_decorations = "RESIZE"

config.font = wezterm.font_with_fallback {
  -- 'Departure Mono',
  -- { family = 'Iosevka Custom', weight = 'Medium' },
  'Berkeley Mono',
  'JetBrains Mono',
}
config.font_size = 18.0

config.keys = {
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentPane { confirm = true },
  },
  {
    key = '=',
    mods = 'CTRL',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = '-',
    mods = 'CTRL',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'ALT',
    action = wezterm.action.DisableDefaultAssignment,
  },
}

config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true

--

return config
