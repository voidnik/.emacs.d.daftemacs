#!/bin/bash

set -o nounset
set -o errexit

# Configures Emacs for building native comp support
#
# https://akrl.sdf.org/gccemacs.html
# https://xenodium.com/trying-out-gccemacs-on-macos/
# https://gist.github.com/AllenDang/f019593e65572a8e0aefc96058a2d23e
# http://www.cesarolea.com/posts/emacs-native-compile/

readonly GCC_DIR="$(realpath $(brew --prefix libgccjit))"
[[ -d $GCC_DIR ]] ||  { echo "${GCC_DIR} not found"; exit 1; }

readonly SED_DIR="$(realpath $(brew --prefix gnu-sed))"
[[ -d $SED_DIR ]] ||  { echo "${SED_DIR} not found"; exit 1; }

readonly GCC_INCLUDE_DIR=${GCC_DIR}/include
[[ -d $GCC_INCLUDE_DIR ]] ||  { echo "${GCC_INCLUDE_DIR} not found"; exit 1; }

readonly GCC_LIB_DIR=${GCC_DIR}/lib/gcc/11
[[ -d $GCC_LIB_DIR ]] ||  { echo "${GCC_LIB_DIR} not found"; exit 1; }

export PATH="${SED_DIR}/libexec/gnubin:${PATH}"
export CFLAGS="-O2 -I${GCC_INCLUDE_DIR}"
export LDFLAGS="-L${GCC_LIB_DIR} -I${GCC_INCLUDE_DIR}"
export LD_LIBRARY_PATH="${GCC_LIB_DIR}"
export DYLD_FALLBACK_LIBRARY_PATH="${GCC_LIB_DIR}"

echo "Environment"
echo "-----------"
echo PATH: $PATH
echo CFLAGS: $CFLAGS
echo LDFLAGS: $LDFLAGS
echo DYLD_FALLBACK_LIBRARY_PATH: $DYLD_FALLBACK_LIBRARY_PATH
echo "-----------"

./autogen.sh

./configure \
     --prefix="$PWD/nextstep/Emacs.app/Contents/MacOS" \
     --enable-locallisppath="${PWD}/nextstep/Emacs.app/Contents/MacOS" \
     --without-dbus \
     --with-gif \
     --with-jpeg \
     --with-png \
     --with-tiff \
     --with-rsvg \
     --with-xft \
     --with-xpm \
     --with-gpm=no \
     --with-json \
     --with-imagemagick \
     --with-mailutils \
     --with-ns \
     --with-cairo \
     --with-xml2 \
     --with-gnutls \
     --with-modules \
     --with-native-compilation \
     --disable-silent-rules \
     --disable-ns-self-contained
