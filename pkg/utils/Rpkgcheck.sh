#! /bin/sh
#
export _R_CHECK_CODOC_S4_METHODS_=true
export _R_CHECK_SUGGESTS_ONLY_=true
#
echo ---------------------------------------------
echo start full package check of $2
echo ---------------------------------------------
#
echo ---------------------------------------------
echo remove old tar.gz of $2
echo ---------------------------------------------
rm -f $2*.tar.gz
#
echo ---------------------------------------------
echo build $2
echo ---------------------------------------------
$1 CMD build --compact-vignettes="gs+qpdf" --resave-data $2
#
tarGz=`ls $2*.tar.gz`
echo ---------------------------------------------
echo tar.gz is $tarGz
echo ---------------------------------------------
#
echo ---------------------------------------------
echo check as cran $tarGz
echo ---------------------------------------------
$1 CMD check --as-cran --timings $tarGz
result=$?
#
echo ---------------------------------------------
echo we test that the check of $2 was successful
echo   i.e. whether the value of check,
echo     which is $result, is 0
echo   before removing the old version
echo   and installing the new version
echo ---------------------------------------------
zero=0
if [ $result -eq $zero ]; then
  #
  echo ---------------------------------------------
  echo REMOVE $2
  echo ---------------------------------------------
  $1 CMD REMOVE $2
  #
  echo ---------------------------------------------
  echo INSTALL $2
  echo ---------------------------------------------
  $1 CMD INSTALL \
    --byte-compile --with-keep.source --compact-docs \
    --resave-data --install-tests --example \
    --html --latex --clean --preclean \
    --compile-both \
    $tarGz
fi

