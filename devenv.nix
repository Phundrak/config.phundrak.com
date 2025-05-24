{ pkgs, ... }:

{
  languages.typescript.enable = true;
  packages = [ pkgs.nodejs_20 ];
  scripts.export.exec = ''
    ${pkgs.emacs}/bin/emacs -Q --script export.el
  '';
}
