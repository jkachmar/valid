{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    cabal-install

    haskellPackages.ghcid
    haskellPackages.ormolu

    (haskell.packages.ghc884.ghcWithPackages (p: with p; [
      deepseq
      lens
      optics
      semigroupoids
    ]))
  ];
}
