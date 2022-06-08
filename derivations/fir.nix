{ mkDerivation, atomic-file-ops, base, binary, bytestring, Cabal
, containers, directory, distributive, fetchgit, filepath
, finite-typelits, generic-monoid, ghc-typelits-knownnat
, ghc-typelits-natnormalise, half, haskus-utils-variant, lens, lib
, mtl, process, split, template-haskell, text, text-short
, transformers, tree-view, typelits-witnesses, vector, vector-sized
}:
mkDerivation {
  pname = "fir";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://gitlab.com/sheaf/fir.git";
    sha256 = "111n8hczwjlixxi91bmfq6f2jss5p7py06yhig43hvb0w5jpddk3";
    rev = "c793223657c66f17e503eb584bd8c4ce04ca613c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    atomic-file-ops base binary bytestring containers directory
    distributive filepath finite-typelits generic-monoid
    ghc-typelits-knownnat ghc-typelits-natnormalise half
    haskus-utils-variant lens mtl process split template-haskell text
    text-short transformers tree-view typelits-witnesses vector
    vector-sized
  ];
  testHaskellDepends = [
    base bytestring Cabal directory filepath process text
  ];
  doCheck = false;
  homepage = "https://gitlab.com/sheaf/fir";
  description = "An EDSL that compiles to SPIR-V for graphics programming on the GPU in Haskell";
  license = lib.licenses.bsd3;
}