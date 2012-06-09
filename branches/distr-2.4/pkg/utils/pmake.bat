@echo off
REM
REM ***pmake*** makes a package...
REM
REM  SYNTAX: pmake <packagename> [R-ver] [Rver2] [+]
REM   * performs R CMD check, R CMD build, R CMD build --binary, R CMD INSTALL to package <packagename>,
REM  with R-ver    the R-version under which the package is to be made; i.e. Rversion is changed
REM          R-ver2  the R-version under which the package is to be checked,
REM                        i.e. Rversion is changed before and after check
REM                        if given installs to R-ver & R-ver2  
REM          +            package is checked agaist R-ver and R-ver2 
REM                        (else just against R-ver2 if given or R-ver if given or preset R-version)
REM           all args are positional, i.e. they only apply if all preceding args are given
REM 
REM To be able to switch between Rversions you need batch utils from G.Grothendieck (on CRAN)
REM which should be on your path. In addition (as for June 8 2012), you need to modify 
REM Rversions.bat from Gabor and add RRversions.bat (from me) to solve 
REM  (a) some change in the policy on where to write registry entries with RSetReg [modified Rversions.bat]
REM  (b) overcome the nasty elevated privileges [RRversions.bat]
REM
REM Second you should place RBuild.bat, RCheck.bat, RInstall.bat, RZip.bat to a place found in the PATH 
REM 
REM IF you want to use word completion, add location of this file to path & remove cd lines
REM 
echo.
set R_ARCH=32
set EL2=call RRversions %2 64
set EL3=call RRversions %3 64
echo %EL2%
echo %EL3%
echo ##########################################
echo This is pmake --- making package %1
echo ##########################################
echo.
cd ..
echo.
if not "%2"=="" (
   echo ----------------------------------------------------------------
   echo R CMD REMOVE under %2
   echo ----------------------------------------------------------------
   %EL2%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
)  else (
   echo ----------------------------------------------------------------
   echo R CMD REMOVE
   echo ----------------------------------------------------------------
)
echo on
call R CMD REMOVE %1
echo off
echo.
echo ----------------------------------------------------------------
if not "%3"=="" ( 
   echo R CMD check under %3
)  else (
   if not "%2"=="" (
      echo R CMD check under %2
   )  else (
      echo R CMD check
   )
) 
echo ----------------------------------------------------------------
if not "%3"=="" (
  %EL3%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
) 
echo on
call Rcheck  %1
echo off
if not "%3"=="" (
   %EL2%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
)
echo.
echo ----------------------------------------------------------------
if "%4"=="+" (
   echo R CMD check under %2
   echo ----------------------------------------------------------------
   echo on
   call RCheck %1 %2
   echo off
   echo.
   echo ----------------------------------------------------------------
)
if not "%2"=="" (
   echo R CMD build under %2
)  else echo R CMD build 
echo ----------------------------------------------------------------
echo on
call RBuild %1
echo off
echo.
echo ----------------------------------------------------------------
if not "%2"=="" (
   echo R CMD INSTALL --build under %2
)  else echo R CMD INSTALL --build 
echo ----------------------------------------------------------------
echo on
call RZip %1
echo off
echo.
if not "%3"=="" (
   echo R CMD INSTALL under %3
   echo ----------------------------------------------------------------
   %EL3%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
   echo on
   call RInstall %1
   echo off
   %EL2%
   PING 1.1.1.1 -n 1 -w 1000 >NUL
   echo ----------------------------------------------------------------
)  else (
   echo ----------------------------------------------------------------
   if not "%2"=="" (
      echo R CMD INSTALL under %2
   ) else echo R CMD INSTALL
   echo ----------------------------------------------------------------
   echo on
   call RInstall %1
   echo off
)
echo ----------------------------------------------------------------
echo FERTIG!!!
echo ----------------------------------------------------------------
cd utils
echo on
