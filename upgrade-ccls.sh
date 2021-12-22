#!/bin/bash
rm -rf ccls
git clone --depth=1 --recursive https://github.com/MaskRay/ccls
cd ccls

if [ "$(uname)" == "Darwin" ]; then
    cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/local/opt/llvm
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    cmake -H. -BRelease -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/lib/llvm-7
else
    exit
fi

cmake --build Release
cd ..
