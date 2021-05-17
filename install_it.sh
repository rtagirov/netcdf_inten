#!/bin/bash
export CXX=''
cd /build/netcdf-3.6.1/src/
./configure --disable-cxx --prefix="$PWD"/../../../lib
make
make check
make install
