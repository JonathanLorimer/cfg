{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:cfg' --allow-eval --warnings";
  testScript = s "tst" "cabal run test:cfg-tests";
  replScript = s "repl" "cabal new-repl lib:cfg";
  formatScript = s "format" "fourmolu -i ./{src,test}/**/*.hs";
  hoogleScript = s "hgl" "hoogle serve";
}
