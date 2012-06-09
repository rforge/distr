@echo off
REM
REM ***pmakeDoc*** makes package distrDoc...
REM
REM  SYNTAX: pmakeDoc [R-ver] [Rver2] [+]
REM   * performs R CMD check, R CMD build, R CMD build --binary, R CMD INSTALL to package distrDoc,
REM   with R-ver    the R-version under which the package is to be made; i.e. Rversion is changed
REM          R-ver2  the R-version under which the package is to be checked,
REM                        i.e. Rversion is changed before and after check
REM                        if given installs to R-ver & R-ver2  
REM          +            package is checked agaist R-ver and R-ver2 
REM                        (else just against R-ver2 if given or R-ver if given or preset R-version)
REM           all args are positional, i.e. they only apply if all preceding args are given
REM 
REM To be able to switch between Rversions you need batch utils from G.Grothendieck (on CRAN)
REM which should be on your path.
REM  (a) some change in the policy on where to write registry entries with RSetReg [modified Rversions.bat]
REM  (b) overcome the nasty elevated privileges [RRversions.bat]
REM
REM Second you should place RBuild.bat, RCheck.bat, RInstall.bat, RZip.bat to a place found in the PATH 
REM 
REM IF you want to use word completion, add location of this file to path & remove cd lines
REM 
echo.
set R_ARCH=32
set EL1=call RRversions %1 64
set EL2=call RRversions %2 64
echo %EL1%
echo %EL2%
echo ##########################################
echo This is pmakeDoc --- making package distrDoc
echo ##########################################
echo.
cd ..
echo.
if not "%1"=="" (
   echo ----------------------------------------------------------------
   echo R CMD REMOVE under %1
   echo ----------------------------------------------------------------
   %EL1%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
)  else (
   echo ----------------------------------------------------------------
   echo R CMD REMOVE
   echo ----------------------------------------------------------------
)
echo on
call R CMD REMOVE distrDoc
echo off
echo.
echo ----------------------------------------------------------------
if not "%2"=="" ( 
   echo R CMD check under %2
)  else (
   if not "%1"=="" (
      echo R CMD check under %1
   ) else (
   echo R CMD check
   )
)
echo ----------------------------------------------------------------
if not "%2"=="" (
   %EL2% 
)
echo on
call RCheck distrDoc
echo off
if not "%2"=="" (
  %EL1%
  PING 1.1.1.1 -n 1 -w 1000 >NUL
)
echo.
echo ----------------------------------------------------------------
if "%3"=="+" (
   echo R CMD check under %1
   echo ----------------------------------------------------------------
   echo on
   call RCheck distrDoc %1
   echo off
   echo.
   echo ----------------------------------------------------------------
)
if not "%1"=="" (
   echo R CMD build under %1
)  else echo R CMD build
echo ----------------------------------------------------------------
echo on
call RBuild distrDoc
echo off
echo.
echo ----------------------------------------------------------------
if not "%1"=="" (
   echo R CMD INSTALL --build under %1
)  else echo R CMD INSTALL --build
echo ----------------------------------------------------------------
echo on
call RZip distrDoc
echo off
echo.
if not "%2"=="" (
   echo ----------------------------------------------------------------
   echo R CMD INSTALL under %2
   echo ----------------------------------------------------------------
   %EL2%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
   echo on
   call RInstall distrDoc
   call echo off
   %EL1%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
)  else (
   echo ----------------------------------------------------------------
   if not "%1"=="" (
      echo R CMD INSTALL under %1
   )  else echo R CMD INSTALL
   echo ----------------------------------------------------------------
   echo on
   call RInstall distrDoc
   echo off
)
echo ----------------------------------------------------------------
echo FERTIG!!!
echo ----------------------------------------------------------------
REM
cd utils
REM
echo on
