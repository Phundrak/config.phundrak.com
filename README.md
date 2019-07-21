![](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)

P’undrak’s dotfiles
===================

This is my collection of dotfiles for my daily GNU/Linux environment, tweaked to my liking. If you wish to get these dotfiles, follow the [1.3](#*Installation).

As you can see, I personally use [zsh](https://github.com/zsh-users/zsh) as my shell of choice, and [Emacs](https://github.com/emacs-mirror/emacs) using [Spacemacs](http://spacemacs.org/) (still with Emacs keybinding) as my editor of choice.

I also use as my desktop environment [i3-gaps](https://github.com/Airblader/i3) + [polybar](https://github.com/jaagr/polybar) + [compton](https://github.com/chjj/compton). You will find some screenshot below.

Features
--------

-   Handy zsh aliases
-   Ready to use Emacs dotfiles optimized for C, Modern C++, Python and Java development
-   Ready to use i3 dotfiles

Dependencies
------------

Because indeed there are some dependencies, and here they are. The versions mentionned are the version I am currently using. My dotfiles **might** work with earlier versions.

-   [Emacs](https://github.com/emacs-mirror/emacs) &gt;= `25.3-1`
    -   [Spacemacs](http://spacemacs.org/) &gt;= `0.200.9@25.3.1`
-   [zsh](https://github.com/zsh-users/zsh) &gt;= `5.0`
-   `.zshrc` dependencies (can be ignored if commenting out the corresponding lines in the file):
    -   [powerline](https://github.com/powerline/powerline) &gt;= `2.6-1`
    -   [clang](http://clang.llvm.org/) &gt;= `5.0.0-1`
    -   [highlight](http://www.andre-simon.de/doku/highlight/highlight.html) &gt;= `3.39-1`
    -   [youtube-dl](http://rg3.github.io/youtube-dl) &gt;= `2017.10.01-1`
    -   [curl](https://curl.haxx.se) &gt;= `7.55.1-2`
    -   [neofetch](https://github.com/dylanaraps/neofetch) &gt;= `3.2.1`
    -   [feh](https://feh.finalrewind.org/) &gt;= `2.20-1`
-   [i3-gaps](https://github.com/Airblader/i3) &gt;= `4.14.1-1`
-   [compton](https://github.com/chjj/compton) &gt;= `0.1_beta2.5-8`
-   [pywal](https://github.com/dylanaraps/pywal) &gt;= `0.6.9-1=`
-   [dmenu](http://tools.suckless.org/dmenu/) &gt;= `4.7-1`
-   [uxterm](http://invisible-island.net/xterm/) &gt;= `330-1`
-   [Rust nightly](https://rustup.rs/) &gt;= `0.20`
-   [eclipse-common](https://eclipse.org) &gt;= `4.7.1-1`
-   [eclipse-java](https://eclipse.org) &gt;= `4.7.1-1`
-   [eclipse-pydev](https://eclipse.org) &gt;= `5.9.2-1`
-   [eclim](http://eclim.org/install.html)
-   [texlive-core](http://tug.org/texlive) &gt;= `2017.44918-1`
-   [minted](https://github.com/gpoore/minted) &gt;= `2.5-1`

Please notice that all of my package manager commands are made for `pacman` (Arch Linux); if you do not use this package manager, do not forget to comment out the related lines in `.zshrc`.

Installation
------------

To install the dotfiles, you have two options. You can either:

-   replace your original dotfiles with mine in their original place.
-   create symbolic links to the dotfiles in the folder where they were downloaded.

The advantage with the latter is you will have all the dotfiles in the same place, plus if at one point you want to get updates from the dotfiles, you just have to download them again at the same place and you won’t have to bother placing them again at the right place.

In any case, if you want to use my dotfiles for Emacs, **please install Spacemacs first**.

Licence
-------

All of my dotfiles (and my dotfiles only) are available under the GNU GPLv3 Licence. Please consult [LICENCE.md](https://github.com/Phundrak/dotfiles/blob/master/LICENSE.md) for more information. In short: you are free to access, edit and redistribute all of my dotfiles under the same licence and as allowed by the licence, and if you fuck up something, it’s your own responsibility.
