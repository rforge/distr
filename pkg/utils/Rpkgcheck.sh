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
echo build
echo ---------------------------------------------
$1 CMD build --compact-vignettes $2
#
echo ---------------------------------------------
echo REMOVE
echo ---------------------------------------------
$1 CMD REMOVE $2
#
echo ---------------------------------------------
echo INSTALL
echo ---------------------------------------------
$1 CMD INSTALL $2
#
echo ---------------------------------------------
echo check
echo ---------------------------------------------
$1 CMD check $2
