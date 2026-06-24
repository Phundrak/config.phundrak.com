{ pkgs, ... }: {
  languages.javascript = {
    enable = true;
    npm.enable = false;
    pnpm.enable = true;
  };
  dotenv.enable = true;
  processes.dev.exec = "pnpm dev";
  cachix = {
    push = "phundrak";
    pull = [
      "phundrak"
      "devenv"
    ];
  };
  tasks = {
    "website:install-deps".exec = "pnpm ci";
    "website:export-md" = {
      exec = "${pkgs.emacs}/bin/emacs -Q --script export.el";
      execIfModified = [ "docs/**/*.org" ];
    };
    "website:build" = {
      exec = "pnpm build";
      after = [
        "website:export-md@succeeded"
        "website:install-deps@succeeded"
      ];
    };
    "website:deploy" = {
      exec = ''
        if [[ -z "''${CLOUDFLARE_API_TOKEN:-}" ]]; then
          echo "Error: CLOUDFLARE_API_TOKEN is not set. Run this in CI only."
          exit 1
        fi
        ${pkgs.wrangler}/bin/wrangler pages deploy
      '';
      after = [ "website:build@succeeded" ];
    };
  };
}
