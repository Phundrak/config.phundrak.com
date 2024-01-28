import { defaultTheme } from '@vuepress/theme-default';
import { viteBundler } from '@vuepress/bundler-vite';
import { defineUserConfig } from 'vuepress';
import { searchProPlugin } from 'vuepress-plugin-search-pro';

import head from './head';

interface ChildPage {
  text: string;
  link: string;
}

const emacsChildPages: ChildPage[] = [
  {
    text: 'Basic Configuration',
    link: '/emacs/basic-config',
  },
  {
    text: 'Custom Elisp',
    link: '/emacs/custom-elisp',
  },
  {
    text: 'Package Manager',
    link: '/emacs/package-manager',
  },
  {
    text: 'Keybindings Managers',
    link: '/emacs/keybinding-managers',
  },
  {
    text: 'Packages - Autocompletion',
    link: '/emacs/packages/autocompletion',
  },
  {
    text: 'Packages - Applications',
    link: '/emacs/packages/applications',
  },
  {
    text: 'Packages - Editing',
    link: '/emacs/packages/editing',
  },
  {
    text: 'Packages - Emacs Built-ins',
    link: '/emacs/packages/emacs-builtin',
  },
  {
    text: 'Packages - Making My Life Easier',
    link: '/emacs/packages/helpful',
  },
  {
    text: 'Packages - LaTeX',
    link: '/emacs/packages/latex',
  },
  {
    text: 'Packages - Org Mode',
    link: '/emacs/packages/org',
  },
  {
    text: 'Packages - Programming',
    link: '/emacs/packages/programming',
  },
  {
    text: 'Packages - Visual Configuration',
    link: '/emacs/packages/visual-config',
  },
  {
    text: 'Packages - Misc',
    link: '/emacs/packages/misc',
  },
  {
    text: 'Keybindings',
    link: '/emacs/keybindings',
  },
];
const stumpwmChildPages: ChildPage[] = [
  {
    text: 'Basic Configuration',
    link: '/stumpwm/init',
  },
  {
    text: 'Colours',
    link: '/stumpwm/colours',
  },
  {
    text: 'Mode-Line',
    link: '/stumpwm/mode-line',
  },
  {
    text: 'Groups and Placement',
    link: '/stumpwm/groups',
  },
  {
    text: 'Theme',
    link: '/stumpwm/theme',
  },
  {
    text: 'Commands',
    link: '/stumpwm/commands',
  },
  {
    text: 'Keybindings',
    link: '/stumpwm/keybindings',
  },
  {
    text: 'Utilities',
    link: '/stumpwm/utilities',
  },
];

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
        children: emacsChildPages,
      },
      '/scripts',
      '/desktop',
      '/fish',
      '/git',
      '/mpd',
      '/neofetch',
      '/picom',
      '/rustfmt',
      {
        text: 'StumpWM',
        link: '/stumpwm/',
        collapsible: true,
        children: stumpwmChildPages,
      },
      '/tmux',
      '/bootstrap',
      {
        text: 'Deprecated Configs',
        link: '/deprecated/',
        collapsible: true,
        children: [
          {
            text: 'AwesomeWM',
            link: '/deprecated/awesome',
          },
          {
            text: 'i3',
            link: '/deprecated/i3',
          },
          {
            text: 'Nano',
            link: '/deprecated/nano',
          },
          {
            text: 'Polybar',
            link: '/deprecated/polybar',
          },
          {
            text: 'Spacemacs',
            link: '/deprecated/spacemacs',
          },
        ],
      },
    ],
  }),
});
