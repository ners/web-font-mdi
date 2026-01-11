{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    ghc-source-gen = {
      url = "github:google/ghc-source-gen";
      flake = false;
    };
    mdi = {
      url = "github:Templarian/MaterialDesign";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" ])
          root;
      };
      pname = "web-font-mdi";
      version = concatStringsSep "." [
        (toString mdi-version.major)
        (toString mdi-version.minor)
        (toString mdi-version.patch)
      ];
      mdi-version = (fromJSON (readFile "${inputs.mdi}/font-build.json")).version;
      haskell-overlay = pkgs:
        with pkgs.haskell.lib.compose;
        hfinal: hprev: {
          "${pname}-gen" =
            let hp = if hprev.ghc.targetPrefix == "" then hfinal else pkgs.haskellPackages; in
            hp.callCabal2nix "${pname}-gen" "${inputs.self}/gen" {
              ghc-source-gen = hp.callCabal2nix "ghc-source-gen" inputs.ghc-source-gen { };
            };
          ${pname} = lib.pipe { } [
            (hfinal.callCabal2nix pname (sourceFilter ./.))
            (overrideCabal (drv: { inherit version; }))
            (drv: drv.overrideAttrs (attrs: {
              postPatch = ''
                ${attrs.postPatch or ""}
                cp "${inputs.mdi}/meta.json" "${inputs.self}/fourmolu.yaml" .
                "${lib.getExe hfinal."${pname}-gen"}" > src/Web/Font/MDI.hs
                sed -i "s/^version:.*/version: ${version}/" web-font-mdi.cabal
              '';
            }))
          ];
        };
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = lib.composeManyExtensions [
            prev.haskell.packageOverrides
            (haskell-overlay prev)
          ];
        };
      };
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = with lib; foldlAttrs
            (acc: name: hp':
              let
                hp = tryEval hp';
                version = getVersion hp.value.ghc;
                majorMinor = versions.majorMinor version;
                ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
              in
              if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.13"
              then acc // { ${ghcName} = hp.value; }
              else acc
            )
            { default = pkgs.haskellPackages; }
            pkgs.haskell.packages;
          libs = pkgs.buildEnv {
            name = "${pname}-libs";
            paths = map (hp: hp.${pname}) (attrValues hps);
            pathsToLink = [ "/lib" ];
          };
          docs = pkgs.haskell.lib.documentationTarball hps.default.${pname};
          sdist = pkgs.haskell.lib.sdistTarball hps.default.${pname};
          docsAndSdist = pkgs.linkFarm "${pname}-docsAndSdist" { inherit docs sdist; };
        in
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          legacyPackages.${system} = pkgs;
          packages.${system}.default = pkgs.symlinkJoin {
            name = "${pname}-all";
            paths = [ libs docsAndSdist ];
            inherit (hps.default.${pname}) meta;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ ps.${pname} ps."${pname}-gen" ];
                nativeBuildInputs = with pkgs'; with haskellPackages; [
                  cabal-gild
                  cabal-install
                  fourmolu
                  hp.haskell-language-server
                ];
                shellHook = ''
                  for f in font-build.json meta.json; do
                    ln -s ${inputs.mdi}/$f .
                  done
                '';
              };
            });
        }
      ) // {
      overlays = {
        default = overlay;
        haskell = haskell-overlay;
      };
    };
}
