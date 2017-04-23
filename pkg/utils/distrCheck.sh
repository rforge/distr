#! /bin/sh
#
export _R_CHECK_CODOC_S4_METHODS_=true
#
echo ---------------------------------------------
echo ---------------------------------------------
echo start full package check of distr-family
echo ---------------------------------------------
#
echo ---------------------------------------------
echo package startupmsg
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build startupmsg
$1 CMD REMOVE startupmsg
$1 CMD INSTALL startupmsg
$1 CMD check startupmsg
echo ---------------------------------------------
echo ---------------------------------------------
echo package SweaveListingUtils
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build SweaveListingUtils
$1 CMD REMOVE SweaveListingUtils
$1 CMD INSTALL SweaveListingUtils
$1 CMD check SweaveListingUtils
echo ---------------------------------------------
echo ---------------------------------------------
echo package distr
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distr
$1 CMD REMOVE distr
$1 CMD INSTALL distr
$1 CMD check distr
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrEx
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrEx
$1 CMD REMOVE distrEx
$1 CMD INSTALL distrEx
$1 CMD check distrEx
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrSim
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrSim
$1 CMD REMOVE distrSim
$1 CMD INSTALL distrSim
$1 CMD check distrSim
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrTEst
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrTEst
$1 CMD REMOVE distrTEst
$1 CMD INSTALL distrTEst
$1 CMD check distrTEst
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrTeach
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrTeach
$1 CMD REMOVE distrTeach
$1 CMD INSTALL distrTeach
$1 CMD check distrTeach
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrMod
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrMod
$1 CMD REMOVE distrMod
$1 CMD INSTALL distrMod
$1 CMD check distrMod
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrEllipse
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrEllipse
$1 CMD REMOVE distrEllipse
$1 CMD INSTALL distrEllipse
$1 CMD check distrEllipse
echo ---------------------------------------------
echo ---------------------------------------------
echo package distrDoc
echo ---------------------------------------------
echo ---------------------------------------------
$1 CMD build distrDoc
$1 CMD REMOVE distrDoc
$1 CMD INSTALL distrDoc
$1 CMD check distrDoc
