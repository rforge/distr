echo off
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
cd .. 
REM 
echo.
echo ##########################################
echo This is pmakeDoc --- making package distrDoc
echo ##########################################
echo.
echo.
echo ----------------------------------------------------------------
echo R CMD REMOVE
echo ----------------------------------------------------------------
echo on
call R CMD REMOVE distrDoc
echo off
echo.
echo ----------------------------------------------------------------
if not "%2"=="" ( 
echo R CMD check under %2
) else (
if not "%1"=="" (
echo R CMD check under %1
) else (
echo R CMD check
)
)
echo ----------------------------------------------------------------
if not "%1"=="" call Rversions %1
if not "%2"=="" call Rversions %2
echo on
call R CMD check distrDoc
echo off
if not "%1"=="" call Rversions %1
echo.
echo ----------------------------------------------------------------
if "%3"=="+" (
echo R CMD check under %1
echo ----------------------------------------------------------------
echo on
call R CMD check --outdir=%1 distrDoc
echo off
echo.
echo ----------------------------------------------------------------
)
if not "%1"=="" (
echo R CMD build under %1
) else echo R CMD build
echo ----------------------------------------------------------------
echo on
call R CMD build distrDoc
echo off
echo.
echo ----------------------------------------------------------------
if not "%1"=="" (
echo R CMD build --binary under %1
) else echo R CMD build --binary 
echo ----------------------------------------------------------------
echo on
call R CMD build --binary --no-vignettes distrDoc
echo off
echo.
echo ----------------------------------------------------------------
if not "%1"=="" (
echo R CMD INSTALL under %1
) else echo R CMD INSTALL
echo ----------------------------------------------------------------
echo on
call R CMD INSTALL distrDoc
echo off
echo ----------------------------------------------------------------
if not "%2"=="" (
echo R CMD INSTALL under %2
echo ----------------------------------------------------------------
call Rversions %2
echo on
call R CMD INSTALL distrDoc
echo off
call Rversions %1
echo ----------------------------------------------------------------
)
echo FERTIG!!!
echo ----------------------------------------------------------------
REM
cd utils
REM
echo on
