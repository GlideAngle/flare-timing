#!/bin/sh

#set +v

cabal install ft-build --overwrite-policy=always --installdir=$HOME/.cabal/bin
ft-build $@
