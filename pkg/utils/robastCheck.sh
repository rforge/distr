#! /bin/sh
#
export _R_CHECK_CODOC_S4_METHODS_=true
#
echo ---------------------------------------------
echo ---------------------------------------------
echo start full package check of RobASt-family
echo ---------------------------------------------
#
echo ---------------------------------------------
echo package RandVar
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build RandVar
$1 CMD REMOVE RandVar
$1 CMD INSTALL RandVar
$1 CMD check RandVar
echo ---------------------------------------------
echo ---------------------------------------------
echo package RobAStBase
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build RobAStBase
$1 CMD REMOVE RobAStBase
$1 CMD INSTALL RobAStBase
$1 CMD check RobAStBase
echo ---------------------------------------------
echo ---------------------------------------------
echo package RobLox
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build RobLox
$1 CMD REMOVE RobLox
$1 CMD INSTALL RobLox
$1 CMD check RobLox
echo ---------------------------------------------
echo ---------------------------------------------
echo package RobLoxBioC
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build RobLoxBioC
$1 CMD REMOVE RobLoxBioC
$1 CMD INSTALL RobLoxBioC
$1 CMD check RobLoxBioC
echo ---------------------------------------------
echo ---------------------------------------------
echo package ROptEst
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build ROptEst
$1 CMD REMOVE ROptEst
$1 CMD INSTALL ROptEst
$1 CMD check ROptEst
echo ---------------------------------------------
echo ---------------------------------------------
echo package ROptEstOld
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build ROptEstOld
$1 CMD REMOVE ROptEstOld
$1 CMD INSTALL ROptEstOld
$1 CMD check ROptEstOld
echo ---------------------------------------------
echo ---------------------------------------------
echo package ROptRegTS
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build ROptRegTS
$1 CMD REMOVE ROptRegTS
$1 CMD INSTALL ROptRegTS
$1 CMD check ROptRegTS
echo ---------------------------------------------
echo ---------------------------------------------
echo package RobRex
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build RobRex
$1 CMD REMOVE RobRex
$1 CMD INSTALL RobRex
$1 CMD check RobRex
