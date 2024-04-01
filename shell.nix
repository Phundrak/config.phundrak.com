{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [
    corepack
  ];
  shellHook = ''
yarn set version stable
'';
}
