import { defaultTheme } from '@vuepress/theme-default';
import { viteBundler } from '@vuepress/bundler-vite';
import { defineUserConfig } from 'vuepress';
import { searchProPlugin } from 'vuepress-plugin-search-pro';

import head from './head';

interface ChildPage {
  text: string;
  link: string;
}

export default defineUserConfig({
  lang: 'en-US',
  title: "Phundrak's Dotfiles",
  head: head,
  description: "Documentation of the GNU/Linux configuration of P'undrak",
  bundler: viteBundler({}),
  markdown: {
    html: false,
    linkify: true,
    typographer: true,
    headers: {
      level: [1, 2, 3, 4, 5],
    },
  },
  plugins: [
    searchProPlugin({
      indexContent: true,
    }),
  ],
  theme: defaultTheme({
    sidebarDepth: 5,
    repo: 'https://labs.phundrak.com/phundrak/config.phundrak.com',
    sidebar: [
      '/',
      '/about',
      {
        text: 'Emacs',
        link: '/emacs/',
        collapsible: true,
        children: [
          '/emacs/basic-config',
          '/emacs/custom-elisp',
          '/emacs/package-manager',
          '/emacs/keybinding-managers',
          '/emacs/packages/autocompletion',
          '/emacs/packages/applications',
          '/emacs/packages/editing',
          '/emacs/packages/emacs-builtin',
          '/emacs/packages/helpful',
          '/emacs/packages/latex',
          '/emacs/packages/org',
          '/emacs/packages/programming',
          '/emacs/packages/visual-config',
          '/emacs/packages/misc',
          '/emacs/keybindings',
        ],
      },
      '/scripts',
      '/desktop',
      '/shell',
      '/fish',
      '/git',
      '/hyprland',
      '/mpd',
      '/tmux',
      {
        text: 'Deprecated Configs',
        link: '/deprecated/',
        collapsible: true,
        children: [
          '/deprecated/awesome',
          '/deprecated/bootstrap',
          '/emacs/packages/exwm',
          '/deprecated/i3',
          '/deprecated/nano',
          '/neofetch',
          '/picom',
          '/deprecated/polybar',
          '/deprecated/spacemacs',
          {
            text: 'StumpWM',
            link: '/stumpwm/',
            collapsible: true,
            children: [
                '/stumpwm/init',
                '/stumpwm/colours',
                '/stumpwm/mode-line',
                '/stumpwm/groups',
                '/stumpwm/theme',
                '/stumpwm/commands',
                '/stumpwm/keybindings',
                '/stumpwm/utilities',
            ],
          },
        ],
      },
    ],
  }),
});
