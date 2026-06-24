import { DefaultTheme } from "vitepress";

export const sidebar: DefaultTheme.Sidebar = {
  '/': [
    { text: 'Index', link: '/' },
    { text: 'Custom Scripts', link: '/scripts' },
    { text: 'Desktop', link: '/desktop' },
    { text: 'General Shell Config', link: '/shell' },
    { text: 'Fish', link: '/fish' },
    { text: 'Git', link: '/git' },
    { text: 'Hyprland', link: '/hyprland' },
    { text: 'MPD', link: '/mpd' },
    { text: 'Tmux', link: '/tmux' },
    {
      text: 'Emacs',
      items: [
        { text: 'Index', link: '/emacs/' },
        { text: 'Basic Config', link: '/emacs/basic-config' },
        { text: 'Custom Elisp', link: '/emacs/custom-elisp' },
        { text: 'Package Managers', link: '/emacs/package-manager' },
        { text: 'Keybind Managers', link: '/emacs/keybinding-managers' },
        { text: 'Autocompletion', link: '/emacs/packages/autocompletion' },
        { text: 'Applications', link: '/emacs/packages/applications' },
        { text: 'Editing', link: '/emacs/packages/editing' },
        { text: 'Built-in', link: '/emacs/packages/emacs-builtin' },
        { text: 'Making My Life Easier', link: '/emacs/packages/helpful' },
        { text: 'LaTeX', link: '/emacs/packages/latex' },
        { text: 'Org Mode', link: '/emacs/packages/org' },
        { text: 'Programming', link: '/emacs/packages/programming' },
        { text: 'Visual Config', link: '/emacs/packages/visual-config' },
        { text: 'Misc', link: '/emacs/packages/misc' },
        { text: 'Keybindings', link: '/emacs/keybindings' },
      ]
    }
  ],
  '/deprecated/': [
    { text: 'AwesomeWM', link: '/deprecated/awesome' },
    { text: 'Bootstrap Script', link: '/deprecated/bootstrap' },
    { text: 'EXWM', link: '/emacs/packages/exwm' },
    { text: 'i3', link: '/deprecated/i3' },
    { text: 'Nano', link: '/deprecated/nano' },
    { text: 'Neofetch', link: '/neofetch' },
    { text: 'Picom', link: '/picom' },
    { text: 'Polybar', link: '/deprecated/polybar' },
    {
      text: 'StumpWM', items: [
        { text: 'Index', link: '/stumpwm/' },
        { text: 'Basic Configuration', link: '/stumpwm/init' },
        { text: 'Colours', link: '/stumpwm/colours' },
        { text: 'Modeline', link: '/stumpwm/mode-line' },
        { text: 'Groups and Placement', link: '/stumpwm/groups' },
        { text: 'Theme', link: '/stumpwm/theme' },
        { text: 'Commands', link: '/stumpwm/commands' },
        { text: 'Keybinds', link: '/stumpwm/keybindings' },
        { text: 'Utilities', link: '/stumpwm/utilities' },
      ]
    }
  ]
}
