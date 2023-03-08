{ mkDerivation, aeson, base, bytestring, containers, directory
, exceptions, extra, HTTP, http-client, http-client-tls, lens, lib
, modern-uri, mtl, parsec, random, text, time
, transformers, uuid,  witherable, csv, stm, geckodriver, nodejs, pkgs
, fetchFromGitHub, callCabal2nix, cryptohash, postgresql-simple, postgresql
, uri-encode
}:

let

  
  
#   scrappySrc = fetchFromGitHub {
#     owner = "Ace-Interview-Prep";
#     repo = "scrappy";
# #    rev = "17bb4b782aa3f01a507349ede6b3af53d9fba7da";
# #    sha256 = "1j8dn9jrbx972hmxjx8ri60h6z5fpmg0v3ijqlsn7qigjw3qbwzf";
#     rev =  "dfa79ce72c921054d4906a833dbfc1e879c24b41";
#     sha256 = "1bl0n8bz461x5dal912768md2y8d8a0h5glgs5fj9hfb4p8kwkfw";
#   };

  nix-thunk = fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "nix-thunk";
    rev = "8fe6f2de2579ea3f17df2127f6b9f49db1be189f";
    sha256 = "14l2k6wipam33696v3dr3chysxhqcy0j7hxfr10c0bxd1pxv7s8b";
  };
  n = import nix-thunk {};

  #gargoylePkgs = import ./deps/gargoyle { haskellPackages = pkgs.haskellPackages; postgresql = pkgs.postgresql; };
  # gargoyle = n.thunkSource "deps/gargoyle";
  # gargoyle-postgresql = n.thunkSource "deps/gargoyle-postgresql";
  # gargoyle-postgresql-connect = repos.gargoyle + "/gargoyle-postgresql-connect";
  # gargoyle-postgresql-nix = repos.gargoyle + "/gargoyle-postgresql-nix";
  
  scrappySrc = n.thunkSource ./deps/scrappy; 
  scrappy = pkgs.haskell.lib.overrideCabal (callCabal2nix "scrappy" scrappySrc {}) {
    librarySystemDepends = [ nodejs ];
  };

    
    
  nodePieces = (import ./node.nix {});
  nodeShell = nodePieces.shell;
  nodeDeps = nodePieces.nodeDependencies;
  nodeWithJSDom = nodePieces.package;

in
mkDerivation {
  pname = "wikiScraper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    cryptohash
    aeson base bytestring containers directory exceptions extra HTTP
    http-client http-client-tls lens modern-uri mtl parsec random
    scrappy
    text time transformers uuid  witherable csv stm geckodriver# nodeDeps
    postgresql-simple #gargoylePkgs.gargoyle-postgresql
    postgresql uuid
    aeson
    uri-encode
  ];
  executableHaskellDepends = [
    cryptohash
    aeson base bytestring containers directory exceptions extra HTTP
    http-client http-client-tls lens modern-uri mtl parsec random
    scrappy
    text time transformers uuid witherable csv stm geckodriver
    postgresql-simple# gargoylePkgs.gargoyle-postgresql
    postgresql uuid
    aeson
    uri-encode
  ];
  testHaskellDepends = [
    cryptohash
    nix-thunk 
    aeson base bytestring containers directory exceptions extra HTTP
    http-client http-client-tls lens modern-uri mtl parsec random
    scrappy
    text time transformers uuid csv stm geckodriver
    postgresql-simple #gargoylePkgs.gargoyle-postgresql
    postgresql uuid
    aeson
    uri-encode
  ];
  librarySystemDepends = [ postgresql ];
  homepage = "TODO";
  license = lib.licenses.bsd3;
}
