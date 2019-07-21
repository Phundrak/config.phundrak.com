# emacs ansi-term support
if test -n "$EMACS"
    set -x TERM eterm-color
end

# this function may be required
function fish_title
    true
end

set -gx PATH $HOME/go/bin $HOME/.cargo/bin $HOME/.local/bin $HOME/.gem/ruby/2.6.0/bin $PATH
set -gx PKG_CONFIG_PATH /usr/local/lib/pkgconfig/
