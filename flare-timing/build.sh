#!/bin/sh

#set +v

stack build --copy-bins
__shake-build/flare-timing-build-cmd $@

