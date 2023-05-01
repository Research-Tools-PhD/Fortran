# cmake script to delete all files in the build directory

#!/bin/bash

rm -rf build/*
mkdir -p build
cd build
cmake ..
cmake --build .
make
ctest -V


