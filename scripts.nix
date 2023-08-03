{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:cfg' --allow-eval --warnings";
  ghcidTestScript = s "dev:tests" "ghcid --command 'cabal new-repl test:cfg-tests' --allow-eval --warnings";
  testScript = s "tst" "cabal run test:cfg-tests";
  doctestScript = s "dtst" "cabal run test:documentation";
  replScript = s "repl" "cabal new-repl lib:cfg";
  formatScript = s "format" "fourmolu -i src && fourmolu -i test";
  formatCheckScript = s "format-check" "fourmolu --mode check src && fourmolu --mode check test";
  hoogleScript = s "hgl" "hoogle serve";
  uploadToHackageScript = s "hkg" ''
    set -u # or set -o nounset
    : "$REF_NAME"
    : "$HACKAGE_USERNAME"
    : "$HACKAGE_PASSWORD"
    TMP_DIR=$(mktemp -d)
    cabal sdist -o "$TMP_DIR"
    cabal upload -u "$HACKAGE_USERNAME" -p "$HACKAGE_PASSWORD" "$TMP_DIR"/cfg-"$REF_NAME".tar.gz
    cabal haddock -o "$TMP_DIR"\ 
      --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs'\ 
      --haddock-hyperlink-source\ 
      --haddock-quickjump\ 
      --haddock-for-hackage
    cabal upload -d -u "$HACKAGE_USERNAME" -p "$HACKAGE_PASSWORD" --publish "$TMP_DIR"/cfg-"$REF_NAME"-docs.tar.gz
    rm -rf "$TMP_DIR"
  '';
}
