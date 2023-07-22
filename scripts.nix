{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:cfg' --allow-eval --warnings";
  testScript = s "test" "cabal run test:cfg-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
