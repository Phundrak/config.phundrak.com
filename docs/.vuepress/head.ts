interface SimplifiedHeader {
  tag: string;
  content: [any];
}

const matomoTrackingCode = `var _paq = window._paq = window._paq || [];
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
  var u="https://matomo.phundrak.com/";
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '2']);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
})();`;

const simplifiedHead = [
  {
    tag: "meta",
    content: [
      {
        name: "author",
        content: "Lucien Cartier-Tilet",
      },
      {
        property: "og:image",
        content: "https://cdn.phundrak.com/img/rich_preview.png",
      },
      {
        property: "og:title",
        content: "P’undrak’s GNU/Linux Config",
      },
      {
        property: "og:description",
        content: "Documentation of P’undrak’s GNU/Linux configuration files",
      },
      {
        name: "twitter:card",
        content: "summary",
      },
      {
        name: "twitter:site",
        content: "@phundrak",
      },
      {
        name: "twitter:creator",
        content: "@phundrak",
      },
      { name: "msapplication-TileColor", content: "#3b4252" },
      { name: "msapplication-TileImage", content: "/ms-icon-144x144.png" },
      { name: "theme-color", content: "#3b4252" },
    ],
  },
  {
    tag: "link",
    content: [
      {
        rel: "apple-touch-icon",
        sizes: "57x57",
        href: "/apple-icon-57x57.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "60x60",
        href: "/apple-icon-60x60.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "72x72",
        href: "/apple-icon-72x72.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "76x76",
        href: "/apple-icon-76x76.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "114x114",
        href: "/apple-icon-114x114.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "120x120",
        href: "/apple-icon-120x120.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "144x144",
        href: "/apple-icon-144x144.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "152x152",
        href: "/apple-icon-152x152.png",
      },
      {
        rel: "apple-touch-icon",
        sizes: "180x180",
        href: "/apple-icon-180x180.png",
      },
      {
        rel: "icon",
        type: "image/png",
        sizes: "192x192",
        href: "/android-icon-192x192.png",
      },
      {
        rel: "icon",
        type: "image/png",
        sizes: "32x32",
        href: "/favicon-32x32.png",
      },
      {
        rel: "icon",
        type: "image/png",
        sizes: "96x96",
        href: "/favicon-96x96.png",
      },
      {
        rel: "icon",
        type: "image/png",
        sizes: "16x16",
        href: "/favicon-16x16.png",
      },
      { rel: "manifest", href: "/manifest.json" },
    ],
  },
];

let head = [];
simplifiedHead.forEach((tag: SimplifiedHeader) => {
  let tagName = tag.tag;
  tag.content.forEach((element) => {
    head.push([tagName, element]);
  });
});
head.push([
  "a",
  { rel: "me", href: "https://mastodon.phundrak.com/@phundrak" },
  "Mastodon",
]);
head.push(["script", {}, matomoTrackingCode]);

export default head;
