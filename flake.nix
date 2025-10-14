{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    mdi = {
      url = "github:Templarian/MaterialDesign";
      flake = false;
    };
  };

  outputs = inputs:
    with builtins;
    let
      pname = "web-font-mdi";
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter (file: any file.hasExt [ "cabal" "hs" "md" ]) root;
      };
      ghcsFor = pkgs: with lib; foldlAttrs
        (acc: name: hp':
          let
            hp = tryEval hp';
            version = getVersion hp.value.ghc;
            majorMinor = versions.majorMinor version;
            ghcName = "ghc${replaceStrings ["."] [""] majorMinor}";
          in
          if hp.value ? ghc && ! acc ? ${ghcName} && versionAtLeast version "9.4" && versionOlder version "9.12"
          then acc // { ${ghcName} = hp.value; }
          else acc
        )
        { }
        pkgs.haskell.packages;
      hpsFor = pkgs: { default = pkgs.haskellPackages; } // ghcsFor pkgs;
      haskell-overlay = hlib: hfinal: hprev: {
        web-font-mdi-gen = hfinal.callCabal2nix "gen" (sourceFilter ./gen) { };
        ${pname} =
          hlib.compose.overrideCabal
            (drv: { version = mdi-version-string; })
            ((hfinal.callCabal2nix pname (sourceFilter ./.) { }).overrideAttrs (attrs: {
              postPatch = ''
                ${attrs.prePatch or ""}
                ${lib.getExe hfinal.web-font-mdi-gen} ${inputs.mdi}/meta.json > src/Web/Font/MDI.hs
                ${lib.optionalString (hprev.ghc.targetPrefix == "" && lib.versionAtLeast hprev.ghc.version "9.10")
                  "${lib.getExe hprev.fourmolu} -i src/Web/Font/MDI.hs"
                }
                sed -i "s/^version:.*/version: ${mdi-version-string}/" web-font-mdi.cabal
              '';
            }));
      };
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (haskell-overlay prev.haskell.lib)
            ];
          };
        })
      ];
      mdi-version = (fromJSON (readFile "${inputs.mdi}/font-build.json")).version;
      mdi-version-string = concatStringsSep "." [
        (toString mdi-version.major)
        (toString mdi-version.minor)
        (toString mdi-version.patch)
      ];
    in
    foreach inputs.nixpkgs.legacyPackages
      (system: pkgs':
        let
          pkgs = pkgs'.extend overlay;
          hps = hpsFor pkgs;
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
            inherit (hps.default.syntax) meta;
          };
          devShells.${system} =
            foreach hps (ghcName: hp: {
              ${ghcName} = hp.shellFor {
                packages = ps: [ hp.web-font-mdi-gen ];
                nativeBuildInputs = [
                  pkgs'.haskellPackages.cabal-install
                  hp.fourmolu
                ] ++ lib.optionals (lib.versionAtLeast hp.ghc.version "9.4") [
                  hp.haskell-language-server
                ];
                shellHook = ''
                  for f in font-build.json meta.json; do
                    ln -s ${inputs.mdi}/$f $f
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
