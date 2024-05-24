{
  description = "LiTS is a library tagging CLI tool";
  inputs = { nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable"; };
  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      packages.${system}.default = pkgs.haskellPackages.mkDerivation {
        pname = "lits";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = with pkgs.haskellPackages; [
          base
          aeson
          bytestring
          directory
          containers
          filepath
          text
          tasty
          tasty-hunit
        ];
        libraryHaskellDepends = with pkgs.haskellPackages; [
          aeson
          base
          bytestring
          containers
          directory
          filepath
          text
        ];
        executableHaskellDepends = with pkgs.haskellPackages; [
          aeson
          base
          bytestring
          directory
          filepath
        ];
        testHaskellDepends = with pkgs.haskellPackages; [
          base
          containers
          directory
          process
          tasty
          tasty-hunit
        ];
        description = "Library Tagging System";
        license = pkgs.lib.licenses.mit;
        mainProgram = "lits";
      };
    };
}
