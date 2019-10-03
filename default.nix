{ mkDerivation, aeson, base, bytestring, containers, hpack, stdenv
, text, utf8-string
}:
mkDerivation {
  pname = "cmake-file-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers text utf8-string
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers text utf8-string
  ];
  testHaskellDepends = [
    aeson base bytestring containers text utf8-string
  ];
  prePatch = "hpack";
  homepage = "https://github.com/umogSlayer/cmake-file-api#readme";
  license = stdenv.lib.licenses.bsd3;
}
