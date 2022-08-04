{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;
  scrappySrc = pkgs.fetchFromGitHub {
    owner = "Ace-Interview-Prep";
    repo = "scrappy";
    rev =  "dfa79ce72c921054d4906a833dbfc1e879c24b41";
    sha256 = "1bl0n8bz461x5dal912768md2y8d8a0h5glgs5fj9hfb4p8kwkfw";
  };
  scrappy = pkgs.haskellPackages.callCabal2nix "scrappy" scrappySrc {}; 
  f = { mkDerivation, base, lib }:
      mkDerivation {
        pname = "crypto-slu";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base scrappy ];
        license = "unknown";
        hydraPlatforms = lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
