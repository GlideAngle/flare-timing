#!/bin/sh

#set +v

mkdir __shake-build
stack build --copy-bins
__shake-build/flight-igc $@
