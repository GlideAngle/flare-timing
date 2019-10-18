#!/bin/sh

#set +v

stack exec pier -- build build-flare-timing
stack exec pier -- run build-flare-timing $@
