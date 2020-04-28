#!/bin/sh

#set +v

cabal v2-test all --test-show-details=direct --test-option="--color=always"
