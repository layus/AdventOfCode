let 
  haskellPackageOverrides = self: super: {
    #Glob = self.callPackage ./Glob-0.7.14.nix {};
  };

  nixpkgs =  import <nixpkgs> { config = {inherit haskellPackageOverrides; }; };

  inherit (nixpkgs) pkgs stdenv lib;

  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    HTTP
    Unique
    ListZipper
    #modular-arithmetic
    #hit-graph
    matrix
    lens
    parsec3-numbers
    vector-space
    linear
    # modular-arithmetic
    numeric-prelude
    complex-generic
    hashmap
    array
  ]);

in stdenv.mkDerivation {
  name = "adventOfCode2017";
  src = ./advent.hs;

  buildInputs =  [ ghc ];

  shellHook = "exec zsh";
}



