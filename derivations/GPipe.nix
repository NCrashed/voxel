{ mkDerivation, base, Boolean, containers, exception-transformers
, gl, hashtables, linear, stdenv, transformers
}:
mkDerivation {
  pname = "GPipe";
  version = "2.2.4";
  sha256 = "409a1f89e7c5f4c6833d8f06ebb1da949fa65bc5bf7a0b58938c7317881c7a50";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base Boolean containers exception-transformers gl hashtables linear
    transformers
  ];
  homepage = "https://github.com/tobbebex/GPipe-Core#readme";
  description = "Typesafe functional GPU graphics programming";
  license = stdenv.lib.licenses.mit;
}
