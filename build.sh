#!/bin/sh

#set +v

cd build
stack build --copy-bins
__shake-build/flare-timing-build-cmd $@
