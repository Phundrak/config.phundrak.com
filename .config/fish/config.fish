# emacs ansi-term support
if test -n "$EMACS"
    set -x TERM eterm-color
end

# this function may be required
function fish_title
    true
end

if test "$TERM" = "dumb"
    function fish_prompt
        echo "\$ "
    end
    function fish_right_prompt; end
    function fish_greeting; end
    function fish_title; end
end

set -gx PATH $HOME/go/bin $HOME/.cargo/bin $HOME/.local/bin $HOME/.gem/ruby/2.6.0/bin $PATH
set -gx PKG_CONFIG_PATH /usr/local/lib/pkgconfig/ $PKG_CONFIG_PATH
set -gx SUDO_ASKPASS ~/.local/bin/askpass

abbr S 'sudo systemctl'
abbr cdebug 'cmake -DCMAKE_BUILD_TYPE=Debug'
abbr chgrp 'chgrp --preserve-root'
abbr chmod 'chmod --preserve-root'
abbr chown 'chown --preserve-root'
abbr clang 'clang -Wall'
abbr clang++ 'clang++ -Wall'
abbr clean clear
abbr compress 'tar -czf'
abbr cp 'cp -i'
abbr cpuinfo lscpu
abbr crelease 'cmake -DCMAKE_BUILD_TYPE=Release'
abbr dc docker-compose
abbr dcd 'docker-compose down'
abbr dcr 'docker-compose run --rm'
abbr dcu 'docker-compose up'
abbr dcub 'docker-compose up --build'
abbr df 'df -H'
abbr diskspace 'sudo df -h | grep -E "sd|lv|Size"'
abbr du 'du -ch'
abbr e 'emacsclient -c'
abbr enw 'emacsclient -c -nw'
abbr exi exit
abbr exti exit
abbr feh 'feh -Zx.'
abbr flac 'youtube-dl -x --audio-format flac --audio-quality 0'
abbr g++ 'g++ -Wall -std=c++17'
abbr gcc 'gcc -Wall -std=c18'
abbr gpumeminfo 'grep -i --color memory /var/log/Xorg.0.log'
abbr hist history
abbr hotp htop
abbr install 'sudo pacman -Sy'
abbr ln 'ln -i'
abbr lns 'ln -si'
abbr lsl 'ls -ahl'
abbr meminfo 'free -m -l -t'
abbr mp3 'youtube-dl -x --audio-format flac --audio-quality 0'
abbr mpv 'mpv --no-border --force-window=no'
abbr mv 'mv -i'
abbr nmcli 'nmcli -p -c auto'
abbr p 'sudo -A pacman'
abbr please 'sudo -A'
abbr pscpu 'ps auxf | sort -nr -k 3'
abbr pscpu10 'ps auxf | sort -nr -k 3 | head -10'
abbr psmem 'ps auxf | sort -nr -k 4'
abbr psmem10 'ps auxf | sort -nr -k 4 | head -10'
abbr purge 'yay -Sc'
abbr q exit
abbr randcommit 'git commit -m (curl -s whatthecommit.com/index.txt)'
abbr remove 'sudo pacman -Rscnd'
abbr rm 'rm -I'
abbr rmd 'rm --preserve-root -Ir'
abbr rmdf 'rm --preserve-root -Irf'
abbr rmf 'rm --preserve-root -If'
abbr s 'systemctl --user'
abbr search 'pacman -Ss'
abbr sudo 'sudo -A'
abbr swipl 'clear && swipl -q && clear'
abbr texhash 'sudo texhash'
abbr tlmgr tllocalmgr
abbr umountC 'cd ~; and sudo umount /media/C'
abbr umountD 'cd; and sudo umount /media/Marpa'
abbr untar 'tar -xvzf'
abbr update 'sudo pacman -Syu'
abbr vi vim
abbr wget 'wget -c'
