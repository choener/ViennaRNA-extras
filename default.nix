with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-BiobaseTypes
    ../Lib-BiobaseXNA
    ../Lib-ViennaRNA-bindings
  ]) // {ViennaRNA-extras = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.ViennaRNA-extras ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      BiobaseTypes
      BiobaseXNA
      ViennaRNA-bindings
    ];
  };
}
