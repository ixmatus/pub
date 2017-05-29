{ mkDerivation, base, bytestring, hedis, optparse-generic, pipes
, pipes-bytestring, stdenv, text
}:
mkDerivation {
  pname = "pub";
  version = "2.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring hedis optparse-generic pipes pipes-bytestring text
  ];
  description = "Pipe stdin to a redis pub/sub channel";
  license = stdenv.lib.licenses.bsd3;
}
