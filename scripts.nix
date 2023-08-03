{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:cfg' --allow-eval --warnings";
  ghcidTestScript = s "dev:tests" "ghcid --command 'cabal new-repl test:cfg-tests' --allow-eval --warnings";
  testScript = s "tst" "cabal run test:cfg-tests";
  doctestScript = s "dtst" "cabal run test:documentation";
  replScript = s "repl" "cabal new-repl lib:cfg";
  formatScript = s "format" "fourmolu -i src && fourmolu -i test";
  formatCheckScript = s "format-check" ''
    fourmolu --mode check src 
    fourmolu --mode check test 
    cabal-fmt -c ./cfg.cabal --Werror 
  '';
  hoogleScript = s "hgl" "hoogle serve";
  uploadToHackageScript = s "hkg" (builtins.readFile ./scripts/hkg.sh);
}
