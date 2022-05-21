{ mkDerivation, base, Boolean, containers, exception-transformers
, gl, hashtables, lib, linear, transformers
}:
mkDerivation {
  pname = "GPipe";
  version = "2.2.5";
  sha256 = "7f3cfe7fdfcedb51695acf5f06557ca358ec590e288e29b14bb4dd5ddaeb90cc";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base Boolean containers exception-transformers gl hashtables linear
    transformers
  ];
  jailbreak = true;
  homepage = "https://github.com/tobbebex/GPipe-Core#readme";
  description = "Typesafe functional GPU graphics programming";
  license = lib.licenses.mit;
}
