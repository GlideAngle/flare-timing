#!/bin/sh

#set +v

cd build
stack build --copy-bins
cd ..
__shake-build/flare-timing-build-cmd $@
