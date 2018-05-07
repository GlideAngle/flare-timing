#!/bin/sh

#set +v

stack build build-flare-timing --copy-bins
__shake-build/build-flare-timing $@
