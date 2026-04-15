{ mkDerivation, atto-lisp, attoparsec, base, bytestring, containers
, deepseq, haskell-src-exts, mtl, parallel, stdenv, text
, transformers, utf8-string
}:
mkDerivation {
  pname = "HaskellEmacs";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    atto-lisp attoparsec base bytestring containers deepseq
    haskell-src-exts mtl parallel text transformers utf8-string
  ];
  license = stdenv.lib.licenses.gpl2;
}
