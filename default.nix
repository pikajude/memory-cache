{ mkDerivation, async, base, bytestring, containers, hashable, keys
, lifted-async, monad-control, random, stdenv, stm, transformers
, unexceptionalio, unordered-containers
}:
mkDerivation {
  pname = "memory-cache";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers hashable keys lifted-async monad-control
    stm transformers unexceptionalio unordered-containers
  ];
  testHaskellDepends = [ async base random ];
  license = stdenv.lib.licenses.bsd3;
}
