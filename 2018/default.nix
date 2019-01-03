let 
  haskellPackageOverrides = self: super: {
    modular-arithmetic = pkgs.haskell.lib.dontCheck super.modular-arithmetic ;
  };

  nixpkgs =  import <nixpkgs> { config = {inherit haskellPackageOverrides; }; };

  inherit (nixpkgs) pkgs stdenv lib;

  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    ListZipper
    Unique
    array
    array-memoize
    complex-generic
    hashmap
    lens
    linear
    matrix
    modular-arithmetic
    mtl
    numeric-prelude
    parsec3-numbers
    sbv
    union-find
    vector-space
  ]);

in stdenv.mkDerivation {
  name = "adventOfCode2018";
  src = ./advent.hs;

  buildInputs =  [ ghc ];

  shellHook = "exec zsh";
}



