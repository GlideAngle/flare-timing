#!/bin/sh

#set +v

mkdir __shake-build
cd build
stack build --copy-bins
cd ..
__shake-build/flight-igc $@
