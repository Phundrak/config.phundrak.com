function fish_title
    true
end

if test -n "$EMACS"
    set -x TERM eterm-color
end

if test "$TERM" = "dumb"
    function fish_prompt
        echo "\$ "
    end
    function fish_right_prompt; end
    function fish_greeting; end
    function fish_title; end
end

function fish_greeting; end

set -gx PATH \
$HOME/.pub-cache/bin $HOME/.local/bin $HOME/go/bin $HOME/.cargo/bin \
$HOME/.gem/ruby/2.6.0/bin $PATH

set -gx SUDO_ASKPASS ~/.local/bin/askpass

set -gx EDITOR emacsclient -c

set -gx DART_SDK /opt/dart-sdk/bin

set -gx ANDROID_HOME $HOME/Android/Sdk

set -gx PKG_CONFIG_PATH /usr/local/lib/pkgconfig/ $PKG_CONFIG_PATH

set -g theme_nerd_fonts_support yes

set -g theme_title_display_process yes

set -g theme_display_hostname ssh

set -g theme_display_sudo_user yes

set -g theme_show_exit_status yes

set -g theme_git_worktree_support yes

abbr df 'df -H'
abbr diskspace 'sudo df -h | grep -E "sd|lv|Size"'

abbr meminfo 'free -m -l -t'

abbr gpumeminfo 'grep -i --color memory /var/log/Xorg.0.log'

abbr cpuinfo lscpu

abbr pscpu 'ps auxf | sort -nr -k 3'
abbr pscpu10 'ps auxf | sort -nr -k 3 | head -10'

abbr psmem 'ps auxf | sort -nr -k 4'
abbr psmem10 'ps auxf | sort -nr -k 4 | head -10'

abbr remove 'sudo pacman -Rscnd'

abbr p 'sudo -A pacman'

abbr purge 'yay -Sc'

abbr search 'pacman -Ss'

abbr update 'sudo pacman -Syu'

abbr s 'systemctl --user'

abbr S 'sudo systemctl'

abbr cdebug 'cmake -DCMAKE_BUILD_TYPE=Debug'
abbr crelease 'cmake -DCMAKE_BUILD_TYPE=Release'

abbr dc docker-compose
abbr dcd 'docker-compose down'
abbr dcr 'docker-compose run --rm'
abbr dcu 'docker-compose up'
abbr dcub 'docker-compose up --build'

abbr swipl 'clear && swipl -q && clear'

abbr e 'emacsclient -c'

abbr enw 'emacsclient -c -nw'

abbr vi vim

abbr tlmgr tllocalmgr

abbr texhash 'sudo texhash'

abbr cp 'cp -i'
abbr ln 'ln -i'
abbr lns 'ln -si'
abbr mv 'mv -i'
abbr rm 'rm -I'
abbr rmd 'rm --preserve-root -Ir'
abbr rmdf 'rm --preserve-root -Irf'
abbr rmf 'rm --preserve-root -If'

abbr chgrp 'chgrp --preserve-root'
abbr chmod 'chmod --preserve-root'
abbr chown 'chown --preserve-root'

abbr clean clear

abbr exi exit
abbr exti exit

abbr hotp htop

abbr please 'sudo -A'

abbr q exit

abbr hist history

abbr flac 'youtube-dl -x --audio-format flac --audio-quality 0 -o "~/Music/%(uploader)s/%(title)s.%(ext)s"'

abbr ytdl 'youtube-dl -f best -ciw -o "~/Videos/YouTube/%(uploader)s/%(upload_date)s - %(title)s.%(ext)s"'

abbr mpv 'mpv --no-border --force-window=no'

abbr compress 'tar -czf'
abbr untar 'tar -xvzf'

abbr feh 'feh -Zx.'

abbr nmcli 'nmcli -p -c auto'

abbr n 'nordvpn'
abbr nc 'nordvpn c'
abbr nd 'nordvpn d'

abbr ncf 'nordvpn c France'
abbr ncg 'nordvpn c Germany'
abbr ncj 'nordvpn c Japan'
abbr ncu 'nordvpn c United_States'

abbr wget 'wget -c'
