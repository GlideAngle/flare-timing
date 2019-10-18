#!/bin/sh

#set +v

cabal new-build build-flare-timing
exec cabal new-run build-flare-timing:build-flare-timing -- "$@"
