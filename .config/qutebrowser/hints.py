# pylint: disable=C0111
from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
config: ConfigAPI = config  # noqa: F821 pylint: disable=E0602,C0103
c: ConfigContainer = c  # noqa: F821 pylint: disable=E0602,C0103

# When a hint can be automatically followed without pressing Enter.
# Type: String
# Valid values:
#   - always: Auto-follow whenever there is only a single hint on a page.
#   - unique-match: Auto-follow whenever there is a unique non-empty match in either the hint string (word mode) or filter (number mode).
#   - full-match: Follow the hint when the user typed the whole hint (letter, word or number mode) or the element's text (only in number mode).
#   - never: The user will always need to press Enter to follow a hint.
c.hints.auto_follow = 'unique-match'

# Duration (in milliseconds) to ignore normal-mode key bindings after a
# successful auto-follow.
# Type: Int
c.hints.auto_follow_timeout = 0

# CSS border value for hints.
# Type: String
c.hints.border = '1px solid #ebcb8b'

# Characters used for hint strings.
# Type: UniqueCharString
c.hints.chars = 'auie,ctsr'

# Dictionary file to be used by the word hints.
# Type: File
c.hints.dictionary = '/usr/share/dict/words'

# Which implementation to use to find elements to hint.
# Type: String
# Valid values:
#   - javascript: Better but slower
#   - python: Slightly worse but faster
c.hints.find_implementation = 'python'

# Hide unmatched hints in rapid mode.
# Type: Bool
c.hints.hide_unmatched_rapid_hints = True

# Leave hint mode when starting a new page load.
# Type: Bool
c.hints.leave_on_load = False

# Minimum number of characters used for hint strings.
# Type: Int
c.hints.min_chars = 2

# Mode to use for hints.
# Type: String
# Valid values:
#   - number: Use numeric hints. (In this mode you can also type letters from the hinted element to filter and reduce the number of elements that are hinted.)
#   - letter: Use the characters in the `hints.chars` setting.
#   - word: Use hints words based on the html elements and the extra words.
c.hints.mode = 'letter'

# Comma-separated list of regular expressions to use for 'next' links.
# Type: List of Regex
c.hints.next_regexes = [
    '\\bnext\\b',
    '\\bmore\\b',
    '\\bnewer\\b',
    '\\b[>→≫]\\b',
    '\\b(>>|»)\\b',
    '\\bcontinue\\b'
]

# Padding (in pixels) for hints.
# Type: Padding
c.hints.padding = {
    'top': 0,
    'bottom': 0,
    'left': 3,
    'right': 3
}

# Comma-separated list of regular expressions to use for 'prev' links.
# Type: List of Regex
c.hints.prev_regexes = [
    '\\bprev(ious)?\\b',
    '\\bback\\b',
    '\\bolder\\b',
    '\\b[<←≪]\\b',
    '\\b(<<|«)\\b'
]

# Rounding radius (in pixels) for the edges of hints.
# Type: Int
c.hints.radius = 3

# Scatter hint key chains (like Vimium) or not (like dwb). Ignored for
# number hints.
# Type: Bool
c.hints.scatter = True

# CSS selectors used to determine which elements on a page should have
# hints.
# Type: Dict
c.hints.selectors = {
    'all': [
        'a',
        'area',
        'textarea',
        'select',
        'input:not([type="hidden"])',
        'button',
        'frame',
        'iframe',
        'img',
        'link',
        'summary',
        '[contenteditable]:not([contenteditable="false"])',
        '[onclick]',
        '[onmousedown]',
        '[role="link"]',
        '[role="option"]',
        '[role="button"]',
        '[ng-click]',
        '[ngClick]',
        '[data-ng-click]',
        '[x-ng-click]',
        '[tabindex]'
    ],
    'links': [
        'a[href]',
        'area[href]',
        'link[href]',
        '[role="link"][href]'
    ],
    'images': [
        'img'
    ],
    'media': [
        'audio',
        'img',
        'video'
    ],
    'url': [
        '[src]',
        '[href]'
    ],
    'inputs': [
        'input[type="text"]',
        'input[type="date"]',
        'input[type="datetime-local"]',
        'input[type="email"]',
        'input[type="month"]',
        'input[type="number"]',
        'input[type="password"]',
        'input[type="search"]',
        'input[type="tel"]',
        'input[type="time"]',
        'input[type="url"]',
        'input[type="week"]',
        'input:not([type])',
        '[contenteditable]:not([contenteditable="false"])',
        'textarea'
    ]}

# Make characters in hint strings uppercase.
# Type: Bool
c.hints.uppercase = False
