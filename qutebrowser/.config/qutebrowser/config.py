# Automatically turn on insert mode when a loaded page focuses a text field
c.input.insert_mode.auto_load = True

# Edit fields in Emacs with Ctrl+E
c.editor.command = ["emacsclient", "+{line}:{column}", "{file}"]

# Make Ctrl+g quit everything like in Emacs
config.bind('<Ctrl-g>', 'leave-mode', mode='insert')
config.bind('<Ctrl-g>', 'leave-mode', mode='command')
config.bind('<Ctrl-g>', 'leave-mode', mode='prompt')
config.bind('<Ctrl-g>', 'leave-mode', mode='hint')

# Tweak some keybindings
config.unbind('d') # Don't close window on lower-case 'd'

# Vim-style movement keys in command mode
config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')

# Open every tab as a new window, Vimb style
c.tabs.tabs_are_windows = True
c.tabs.show = "multiple"
c.tabs.last_close = "close"

c.auto_save.session = True
c.scrolling.smooth = True
c.session.lazy_restore = True
c.content.autoplay = False

# Scale pages and UI better for hidpi
c.zoom.default = "90%"

c.statusbar.show = "in-mode"

# Use dark mode where possible
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.prefers_color_scheme_dark = True

c.colors.webpage.bg = "#1e1e1e"
c.colors.completion.category.bg = "#252525"
c.colors.completion.item.selected.bg = "#123862"
c.colors.completion.even.bg = "#1e1e1e"
c.colors.completion.odd.bg = "#1e1e1e"
c.colors.keyhint.bg = "#1e1e1e"
c.colors.prompts.bg = "#1e1e1e"
c.colors.statusbar.command.bg = "#1e1e1e"
c.colors.tabs.bar.bg = "#1e1e1e"

c.colors.completion.scrollbar.bg = "#1e1e1e"
c.colors.completion.scrollbar.fg = "#7463cd"

c.colors.completion.item.selected.fg = "#d4d4d4"
c.colors.completion.category.fg = "#d4d4d4"
c.colors.completion.fg = "#d4d4d4"
c.colors.downloads.error.fg = "#d4d4d4"
c.colors.downloads.start.fg = "#d4d4d4"
c.colors.downloads.stop.fg = "#d4d4d4"
c.colors.hints.fg = "#1e1e1e"
c.colors.messages.error.fg = "#d4d4d4"
c.colors.messages.info.fg = "#d4d4d4"
c.colors.messages.warning.fg = "#d4d4d4"
c.colors.prompts.fg = "#d4d4d4"
c.colors.statusbar.caret.fg = "#d4d4d4"
c.colors.statusbar.caret.selection.fg = "#d4d4d4"
c.colors.statusbar.command.fg = "#d4d4d4"
c.colors.statusbar.command.private.fg = "#d4d4d4"
c.colors.statusbar.insert.fg = "#d4d4d4"
c.colors.statusbar.normal.fg = "#d4d4d4"
c.colors.statusbar.passthrough.fg = "#d4d4d4"
c.colors.statusbar.private.fg = "#d4d4d4"
c.colors.statusbar.url.fg = "#d4d4d4"
c.colors.tabs.even.fg = "#d4d4d4"
c.colors.tabs.odd.fg = "#d4d4d4"
c.colors.tabs.pinned.even.fg = "#d4d4d4"
c.colors.tabs.pinned.odd.fg = "#d4d4d4"
c.colors.tabs.pinned.selected.even.fg = "#d4d4d4"
c.colors.tabs.pinned.selected.odd.fg = "#d4d4d4"
c.colors.tabs.selected.even.fg = "#d4d4d4"
c.colors.tabs.selected.odd.fg = "#d4d4d4"

c.colors.completion.category.border.bottom = "#1e1e1e"
c.colors.completion.category.border.top = "#1e1e1e"
c.colors.prompts.border = "#1e1e1e"
c.colors.completion.item.selected.border.bottom = "#123862"
c.colors.completion.item.selected.border.top = "#123862"

config.load_autoconfig()
