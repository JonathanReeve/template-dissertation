{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          # Add compile-time Haskell build dependencies here
          haskell-language-server
          lucid clay shake regex-compat text-regex-replace with-utf8
          pandoc
          pandoc-crossref
          warp wai-app-static # Dev webserver
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  propagatedBuildInputs = with pkgs; [
    # Add any command-line dependiencies you have, here.
    git # Needed for VC
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
