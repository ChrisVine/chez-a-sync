#!/bin/sh

# set LD_LIBRARY_PATH so that event-loop.ss and files depending on it
# will compile, so they can see libchez-a-sync-0.so/dylib
LD_LIBRARY_PATH=../lib:../lib/.libs:$LD_LIBRARY_PATH
# cater for Mac OS
DYLD_LIBRARY_PATH=../lib:../lib/.libs:$DYLD_LIBRARY_PATH
CHEZSCHEMELIBDIRS=".."

export LD_LIBRARY_PATH DYLD_LIBRARY_PATH CHEZSCHEMELIBDIRS
echo "(compile-library \"$1\" \"$2\")" | scheme -q
