with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    BiobaseENA = ../Lib-BiobaseENA;
    BiobaseTypes = ../Lib-BiobaseTypes;
    BiobaseXNA = ../Lib-BiobaseXNA;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
    ViennaRNA-bindings = ../Lib-ViennaRNA-bindings;
    ViennaRNA-extras = ./.;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.ViennaRNA-extras ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      BiobaseTypes BiobaseXNA
      DPutils
      ForestStructures
      OrderedBits
      PrimitiveArray
      SciBaseTypes
      ViennaRNA-bindings
    ];
  };
}
