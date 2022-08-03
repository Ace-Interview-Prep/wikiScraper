{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;
  scrappySrc = pkgs.fetchFromGitHub {
    owner = "Ace-Interview-Prep";
    repo = "scrappy";
    rev =  "17bb4b782aa3f01a507349ede6b3af53d9fba7da";
    sha256 = "1j8dn9jrbx972hmxjx8ri60h6z5fpmg0v3ijqlsn7qigjw3qbwzf";
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
