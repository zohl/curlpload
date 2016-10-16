{ mkDerivation, aeson, base, bytestring, case-conversion, cmdargs
, containers, data-default, directory, filepath, hsyslog, ini
, postgresql-simple, postgresql-simple-bind, servant
, servant-server, stdenv, text, time, wai, warp, warp-autoquit
, warp-socket-activation
}:
mkDerivation {
  pname = "curlpload";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring case-conversion cmdargs containers
    data-default directory filepath hsyslog ini postgresql-simple
    postgresql-simple-bind servant servant-server text time wai warp
    warp-autoquit warp-socket-activation
  ];
  description = "A simple server to store texts and binary data uploaded via curl";
  license = stdenv.lib.licenses.bsd3;
}
