export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export EMAIL="lucien@phundrak.com"
export NAME="Lucien Cartier-Tilet"
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/usr/local/lib"
export LSP_USE_PLISTS=true

export HISTFILE="${XDG_STATE_HOME}/bash/history"
export _Z_DATA="$XDG_DATA_HOME"/z
export WINEPREFIX="$XDG_DATA_HOME"/wine
export TEXMFVAR="$XDG_CACHE_HOME"/texlive/texmf-var
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle
export PYLINTHOME="${XDG_CACHE_HOME}"/pylint
export PYENV_ROOT="$XDG_DATA_HOME"/pyenv
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export PARALLEL_HOME="$XDG_CONFIG_HOME"/parallel
export _JAVA_OPTIONS="-Djava.util.prefs.userRoot=${XDG_CONFIG_HOME}/java"
export NVM_DIR="$XDG_DATA_HOME"/nvm
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export __GL_SHADER_DISK_CACHE_PATH="$XDG_CACHE_HOME"/nv
export NUGET_PACKAGES="$XDG_CACHE_HOME"/NuGetPackages
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
export TERMINFO="$XDG_DATA_HOME"/terminfo
export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo
export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
export ICEAUTHORITY="$XDG_CACHE_HOME"/ICEauthority
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
export GOPATH="$XDG_DATA_HOME"/go
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export GEM_HOME="${XDG_DATA_HOME}"/gem
export GEM_SPEC_CACHE="${XDG_CACHE_HOME}"/gem
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
export CARGO_HOME="$XDG_DATA_HOME"/cargo

export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"

export GTK_THEME="Nordic"
export GTK_ICON_THEME="Flat-Remix-Dark"

export DART_SDK=/opt/dart-sdk/bin
export ANDROID_HOME="$HOME/Android/Sdk"
export CHROME_EXECUTABLE=/usr/bin/chromium

export DENO_DIR="$HOME/.config/deno"
export DENO_INSTALL_ROOT="$HOME/.local/bin/deno"

export SUDO_ASKPASS="$HOME/.local/bin/askpass"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

export BROWSER=firefox
export XMODIFIERS=

LESS_TERMCAP_mb="$(printf '\e[1;32m')"
LESS_TERMCAP_md="$(printf '\e[1;32m')"
LESS_TERMCAP_me="$(printf '\e[0m')"
LESS_TERMCAP_se="$(printf '\e[0m')"
LESS_TERMCAP_so="$(printf '\e[01;33m')"
LESS_TERMCAP_ue="$(printf '\e[0m')"
LESS_TERMCAP_us="$(printf '\e[1;4;31m')"
export LESS_TERMCAP_mb
export LESS_TERMCAP_md
export LESS_TERMCAP_me
export LESS_TERMCAP_se
export LESS_TERMCAP_so
export LESS_TERMCAP_ue
export LESS_TERMCAP_us

alias wget='wget --hsts-file=$XDG_DATA_HOME/wget-hsts'

PATH="/usr/lib/xfce-polkit/:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
PATH="$GEM_HOME/ruby/2.6.0/bin:$PATH"
PATH="$GEM_HOME/ruby/2.6.0/bin:$PATH"
PATH="$GOPATH/bin:$PATH"
PATH="${CARGO_HOME}/bin:$PATH"
PATH="$HOME/Android/Sdk/tools/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH
