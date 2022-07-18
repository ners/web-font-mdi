{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    mdi = {
      url = "github:Templarian/MaterialDesign";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);
      mdi-version = (fromJSON (readFile "${inputs.mdi}/font-build.json")).version;
      mdi-version-string = concatStringsSep "." [
        (toString mdi-version.major)
        (toString mdi-version.minor)
        (toString mdi-version.patch)
      ];
      web-font-mdi = (haskellPackages.callCabal2nix "web-font-mdi" ./. { }).overrideAttrs (attrs: {
        version = mdi-version-string;
        patchPhase = ''
          cp ${inputs.mdi}/meta.json .
          sed -i "s/version:.*/version: ${mdi-version-string}/" package.yaml
          cat package.yaml
        '';
      });
    in
    {
      packages = {
        inherit web-font-mdi;
        default = web-font-mdi;
      };
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          (haskellPackages.ghcWithPackages (ps: haskellDeps web-font-mdi))
          haskellPackages.cabal-install
          haskellPackages.fourmolu
          haskellPackages.haskell-language-server
          haskellPackages.hpack
        ];
      };
    }
  );
}
