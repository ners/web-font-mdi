{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    mdi = {
      url = "github:Templarian/MaterialDesign";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      mdi-version = (fromJSON (readFile "${inputs.mdi}/font-build.json")).version;
      mdi-version-string = concatStringsSep "." [
        (toString mdi-version.major)
        (toString mdi-version.minor)
        (toString mdi-version.patch)
      ];
      src = pkgs.buildEnv {
        name = "source";
        paths = [
          (
            inputs.nix-filter.lib {
              root = ./.;
              include = [ "src" "util" "package.yaml" "LICENCE" ];
            }
          )
          (
            inputs.nix-filter.lib {
              root = inputs.mdi;
              include = [ "meta.json" ];
            }
          )
        ];
        postBuild = ''
          sed -i "s/version:.*/version: ${mdi-version-string}/" $out/package.yaml
        '';
      };
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          web-font-mdi = self.callCabal2nix "web-font-mdi" src { };
        };
      };
    in
    {
      packages = with haskellPackages; {
        inherit web-font-mdi;
        default = web-font-mdi;
      };
      devShells.default = haskellPackages.shellFor {
        packages = ps: with ps; [ web-font-mdi ];
        nativeBuildInputs = with haskellPackages; [
          cabal-install
          fourmolu
          haskell-language-server
          hpack
        ];
      };
    }
  );
}
