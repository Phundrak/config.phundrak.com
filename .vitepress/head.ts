interface Head {
  type?: 'image/png';
  async?: boolean,
  src?: string,
  'data-website-id'?: string
}

interface Favicon extends Head {
  rel: 'apple-touch-icon' | 'icon' | 'manifest';
  href: string;
  sizes?: string;
}

interface Meta extends Head {
  property?: string,
  name?: string,
  content: string
}

const favicons: Favicon[] = [
  { rel: 'apple-touch-icon', sizes: '57x57', href: '/img/meta/apple-icon-57x57.png' },
  { rel: 'apple-touch-icon', sizes: '60x60', href: '/img/meta/apple-icon-60x60.png' },
  { rel: 'apple-touch-icon', sizes: '72x72', href: '/img/meta/apple-icon-72x72.png' },
  { rel: 'apple-touch-icon', sizes: '76x76', href: '/img/meta/apple-icon-76x76.png' },
  { rel: 'apple-touch-icon', sizes: '114x114', href: '/img/meta/apple-icon-114x114.png' },
  { rel: 'apple-touch-icon', sizes: '120x120', href: '/img/meta/apple-icon-120x120.png' },
  { rel: 'apple-touch-icon', sizes: '144x144', href: '/img/meta/apple-icon-144x144.png' },
  { rel: 'apple-touch-icon', sizes: '152x152', href: '/img/meta/apple-icon-152x152.png' },
  { rel: 'apple-touch-icon', sizes: '180x180', href: '/img/meta/apple-icon-180x180.png' },
  { rel: 'icon', type: 'image/png', sizes: '192x192', href: '/img/meta/android-icon-192x192.png' },
  { rel: 'icon', type: 'image/png', sizes: '32x32', href: '/img/meta/favicon-32x32.png' },
  { rel: 'icon', type: 'image/png', sizes: '96x96', href: '/img/meta/favicon-96x96.png' },
  { rel: 'icon', type: 'image/png', sizes: '16x16', href: '/img/meta/favicon-16x16.png' },
  { rel: 'manifest', href: '/img/meta/manifest.json' },
];

const meta: Meta[] = [
  {
    name: 'author',
    content: 'Lucien Cartier-Tilet',
  },
  {
    property: 'og:image',
    content: 'https://cdn.phundrak.com/img/rich_preview.png',
  },
  {
    property: 'og:title',
    content: 'P’undrak’s Dotfiles',
  },
  {
    property: 'og:description',
    content: 'P’undrak’s Linux configuration',
  },
  {
    name: 'fediverse:creator',
    content: '@phundrak@mastodon.phundrak.com',
  },
  {
    name: 'twitter:card',
    content: 'summary',
  },
  {
    name: 'twitter:site',
    content: '@phundrak',
  },
  {
    name: 'twitter:creator',
    content: '@phundrak',
  },
  { name: 'msapplication-TileColor', content: '#3b4252' },
  { name: 'msapplication-TileImage', content: '/ms-icon-144x144.png' },
  { name: 'theme-color', content: '#3b4252' },

]

const appHead: (string | Head)[][] = favicons.map((head) => ['link', head])
meta.map((item) => appHead.push(['meta', item]))

export default appHead;
