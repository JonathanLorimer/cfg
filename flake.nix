{
  description = "cfg";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: 
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          # "aarch64-linux"
          # "x86_64-darwin"
          # "aarch64-darwin"
        ] (system: function rec {
          inherit system;
          latestCompilerVersion = "ghc982";
          compilerVersions = [
            "ghc964"
            latestCompilerVersion
          ];
          pkgs = nixpkgs.legacyPackages.${system};
          overrides = hfinal: hprev: {
            cfg = hfinal.callCabal2nix "cfg" ./. {};
          };
          
          hsPkgs = pkgs.lib.lists.foldr (cv: hp: 
            { 
              "${cv}" = pkgs.haskell.packages.${cv}.override {
                inherit overrides;
              };
            } // hp
          ) {} compilerVersions;
        });
    in
    {
      # nix fmt
      formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

      # nix develop
      devShells = forAllSystems ({latestCompilerVersion, hsPkgs, pkgs, system, ...}: 
      let hp = hsPkgs."${latestCompilerVersion}";
      in {
        default = 
          hp.shellFor {
            packages = p: [
              p.cfg
            ];
            buildInputs = with pkgs;
              [
                hp.haskell-language-server
                haskellPackages.cabal-install
                cabal2nix
                hp.ghcid
                hp.fourmolu_0_15_0_0
                hp.cabal-fmt
                nodePackages_latest.serve
                hp.doctest
              ]
              ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
          };
        ci =
          hp.shellFor {
            packages = p: [
              p.cfg
            ];
            buildInputs = with pkgs;
              [
                haskellPackages.cabal-install
                haskellPackages.cabal-fmt
                hp.fourmolu_0_15_0_0
              ]
              ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
        };
      });

      # nix build
      packages = forAllSystems ({pkgs, hsPkgs, latestCompilerVersion, compilerVersions, ...}: with pkgs.lib;
        { default = hsPkgs."${latestCompilerVersion}".cfg; 
          printCompilerVersions = pkgs.writeShellScriptBin "printCompilerVersions" (lists.foldr (cv: acc: "echo ${cv};" + acc) "" compilerVersions);
        } 
        // attrsets.mapAttrs' (cv: hp: attrsets.nameValuePair "cfg${strings.toUpper cv}" hp.cfg) hsPkgs
      );

      checks = forAllSystems ({pkgs, hsPkgs, latestCompilerVersion, compilerVersions, ...}: with pkgs.lib;
        attrsets.mapAttrs' (cv: hp: attrsets.nameValuePair "cfg${strings.toUpper cv}" hp.cfg) hsPkgs
      );

      # nix run
      apps = forAllSystems ({system, ...}: {
        printCompilerVersions = { 
          type = "app"; 
          program = "${self.packages.${system}.printCompilerVersions}/bin/printCompilerVersions"; 
        };
        default = self.apps.${system}.printCompilerVersions;
      });
    };
}
