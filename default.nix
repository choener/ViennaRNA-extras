{ mkDerivation, array, attoparsec, base, BiobaseTypes, BiobaseXNA
, bytestring, data-default-class, deepseq, lens, lib, parallel
, QuickCheck, streaming, streaming-bytestring, strict
, strict-base-types, tasty, tasty-quickcheck, tasty-th, vector
, ViennaRNA-bindings
}:
mkDerivation {
  pname = "ViennaRNA-extras";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    array attoparsec base BiobaseTypes BiobaseXNA bytestring
    data-default-class deepseq lens QuickCheck streaming
    streaming-bytestring strict strict-base-types ViennaRNA-bindings
  ];
  testHaskellDepends = [
    attoparsec base BiobaseTypes bytestring data-default-class lens
    parallel QuickCheck tasty tasty-quickcheck tasty-th vector
  ];
  homepage = "https://github.com/choener/ViennaRNA-extras";
  description = "ViennaRNA v2 extensions";
  license = lib.licenses.bsd3;
}
