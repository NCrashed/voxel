{ mkDerivation, base, Boolean, containers, exception-transformers
, fetchgit, gl, hashtables, lib, linear, transformers
}:
mkDerivation {
  pname = "GPipe";
  version = "2.4.0";
  src = fetchgit {
    url = "https://github.com/NCrashed/GPipe-Core.git";
    sha256 = "0vyc9i7jwvsdal01w7w98baspk0srvzx8ws4zzhwqhcwrwkzgqqn";
    rev = "7ab6ef7d453aa930298f1466d0c0ffbd87280b9f";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/GPipe-Core; echo source root reset to $sourceRoot";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base Boolean containers exception-transformers gl hashtables linear
    transformers
  ];
  homepage = "https://github.com/tobbebex/GPipe-Core#readme";
  description = "Typesafe functional GPU graphics programming";
  license = lib.licenses.mit;
}