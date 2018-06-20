#!/bin/bash

arch="$1"
version=$(grep "^version" curl-runnings.cabal | sed 's/ //g'  | cut -d ':' -f 2)
archive="curl-runnings-$version-$arch.tar.gz"

echo "Creating $archive"

stack install
cp ~/.local/bin/curl-runnings .
strip curl-runnings
tar -czvf "$archive" curl-runnings

echo "Done!"
