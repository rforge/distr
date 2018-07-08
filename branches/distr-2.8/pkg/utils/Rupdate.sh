#! /bin/sh
echo *********************************************
echo start updating via rsync
echo *********************************************
$1/tools/rsync-recommended
$1/tools/link-recommended
#
#
#
echo *********************************************
echo start configure
echo *********************************************
$1/configure --with-blas="-lf77blas -latlas" --enable-R-shlib
#
#
#
export _R_CHECK_CODOC_S4_METHODS_=true
#export R_COMPILE_PKGS=1
#export R_COMPILER_SUPPRESS_ALL=1
echo *********************************************
echo start make process
echo *********************************************
make
make check
make vignettes
