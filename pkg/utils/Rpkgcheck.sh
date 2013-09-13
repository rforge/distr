#! /bin/sh
#
export _R_CHECK_CODOC_S4_METHODS_=true
export _R_CHECK_SUGGESTS_ONLY_=true
#
echo ---------------------------------------------
echo start full package check
echo ---------------------------------------------
#
echo ---------------------------------------------
echo remove old tar.gz
echo ---------------------------------------------
rm -f $2*.tar.gz
#
echo ---------------------------------------------
echo build
echo ---------------------------------------------
$1 CMD build --compact-vignettes="gs+qpdf" --resave-data $2
#
tarGz=`ls $2*.tar.gz`
echo ---------------------------------------------
echo tar.gz is $tarGz
echo ---------------------------------------------
#
echo ---------------------------------------------
echo check as cran
echo ---------------------------------------------
$1 CMD check --as-cran --timings $tarGz
#
echo ---------------------------------------------
echo REMOVE
echo ---------------------------------------------
$1 CMD REMOVE $2
#
echo ---------------------------------------------
echo INSTALL
echo ---------------------------------------------
$1 CMD INSTALL \
  --byte-compile --with-keep.source --compact-docs \
  --resave-data --install-tests --example \
  --html --latex --clean --preclean \
  --compile-both \
  $tarGz

