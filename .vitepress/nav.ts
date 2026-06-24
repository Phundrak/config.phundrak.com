import { DefaultTheme } from "vitepress";

export const nav: DefaultTheme.NavItem[] = [
  { text: 'About', link: '/about' },
  { text: 'Configs', link: '/' },
  { text: 'Deprecated Configs', link: '/deprecated/', activeMatch: '/deprecated/*' },
  { text: 'Source', link: 'https://labs.phundrak.com/phundrak/config.phundrak.com' },
]
