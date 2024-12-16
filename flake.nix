{
  description = "cfg";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ] (system:
        function rec {
          inherit system;
          pkgs = nixpkgs.legacyPackages.${system};
          overrides = hfinal: hprev: {
            cfg = hfinal.callCabal2nix "cfg" ./. {};
          };
          hsPkgs = pkgs.haskellPackages.override {
            inherit overrides;
          };
        });
  in {
    # nix fmt
    formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

    # nix develop
    devShells = forAllSystems ({
      hsPkgs,
      pkgs,
      system,
      ...
    }: {
      default = hsPkgs.shellFor {
        packages = p: [
          p.cfg
        ];
        buildInputs = with pkgs;
          [
            hsPkgs.haskell-language-server
            haskellPackages.cabal-install
            cabal2nix
            hsPkgs.ghcid
            hsPkgs.fourmolu
            hsPkgs.cabal-fmt
            hsPkgs.doctest
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      };
      ci = hsPkgs.shellFor {
        packages = p: [
          p.cfg
        ];

        buildInputs = with pkgs;
          [
            haskellPackages.cabal-install
            haskellPackages.cabal-fmt
            hsPkgs.fourmolu_0_15_0_0
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      };
    });

    # nix build
    packages = forAllSystems (
      {
        pkgs,
        hsPkgs,
        ...
      }: {
        default = hsPkgs.cfg;
      }
    );
  };
}
