# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config: ConfigAPI = config  # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # noqa: F821 pylint: disable=E0602,C0103

# Hide the window decoration.  This setting requires a restart on
# Wayland.
# Type: Bool
c.window.hide_decoration = True

# Format to use for the window title. The same placeholders like for
# `tabs.title.format` are defined.
# Type: FormatString
c.window.title_format = '{current_title}{title_sep}qutebrowser'

# Set the main window background to transparent.  This allows having a
# transparent tab- or statusbar (might require a compositor such as
# picom). However, it breaks some functionality such as dmenu embedding
# via its `-w` option. On some systems, it was additionally reported
# that main window transparency negatively affects performance.  Note
# this setting only affects windows opened after setting it.
# Type: Bool
c.window.transparent = True
