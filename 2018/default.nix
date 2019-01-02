let 
  haskellPackageOverrides = self: super: {
    modular-arithmetic = pkgs.haskell.lib.dontCheck super.modular-arithmetic ;
  };

  nixpkgs =  import <nixpkgs> { config = {inherit haskellPackageOverrides; }; };

  inherit (nixpkgs) pkgs stdenv lib;

  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    HTTP
    Unique
    ListZipper
    matrix
    lens
    parsec3-numbers
    vector-space
    linear
    modular-arithmetic
    numeric-prelude
    complex-generic
    hashmap
    array
    mtl
    array-memoize
    linear
    sbv
    union-find
  ]);

in stdenv.mkDerivation {
  name = "adventOfCode2017";
  src = ./advent.hs;

  buildInputs =  [ ghc ];

  shellHook = "exec zsh";
}



