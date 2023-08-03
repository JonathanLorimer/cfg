set -u # or set -o nounset
set -x
set -e
: "$REF_NAME"
: "$HACKAGE_USERNAME"
: "$HACKAGE_PASSWORD"
TMP_DIR=$(mktemp -d)
cabal sdist -o "$TMP_DIR"
cabal upload -u "$HACKAGE_USERNAME" -p "$HACKAGE_PASSWORD" "$TMP_DIR"/cfg-"$REF_NAME".tar.gz
cabal haddock --builddir "$TMP_DIR" --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --haddock-hyperlink-source --haddock-quickjump --haddock-for-hackage
cabal upload -d -u "$HACKAGE_USERNAME" -p "$HACKAGE_PASSWORD" "$TMP_DIR"/cfg-"$REF_NAME"-docs.tar.gz
rm -rf "$TMP_DIR"
