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
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function rec {
          inherit system;
          compilerVersion = "ghc962";
          pkgs = nixpkgs.legacyPackages.${system};
          
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev: {
              cfg = hfinal.callCabal2nix "cfg" ./. {};
              doctest = hfinal.doctest_0_21_1;
            };
          };
        });
    in
    {
      # nix fmt
      formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

      # nix develop
      devShells = forAllSystems ({hsPkgs, pkgs, system, ...}: {
        default = 
          hsPkgs.shellFor {
            packages = p: [
              p.cfg
            ];
            buildInputs = with pkgs;
              [
                hsPkgs.haskell-language-server
                haskellPackages.cabal-install
                cabal2nix
                haskellPackages.ghcid
                hsPkgs.fourmolu_0_13_0_0
                haskellPackages.cabal-fmt
                nodePackages_latest.serve
                hsPkgs.doctest_0_21_1
              ]
              ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
        };
        ci =
          hsPkgs.shellFor {
            packages = p: [
              p.cfg
            ];
            buildInputs = with pkgs;
              [
                haskellPackages.cabal-install
                haskellPackages.cabal-fmt
                hsPkgs.fourmolu_0_13_0_0
              ]
              ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
        };
      });

      # nix build
      packages = forAllSystems ({hsPkgs, ...}: {
          cfg = hsPkgs.cfg;
          default = hsPkgs.cfg;
          inherit hsPkgs;
      });

      # You can't build the cfg package as a check because of IFD in cabal2nix
      checks = {};

      # nix run
      apps = forAllSystems ({system, ...}: {
        cfg = { 
          type = "app"; 
          program = "${self.packages.${system}.cfg}/bin/cfg"; 
        };
        default = self.apps.${system}.cfg;
      });
    };
}
