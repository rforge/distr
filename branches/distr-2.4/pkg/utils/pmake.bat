echo off
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
REM 
REM 
REM ALSO add the utils folder to your path
REM 
echo.
echo ##########################################
echo This is pmake --- making package %1
echo ##########################################
echo.
echo.
echo ----------------------------------------------------------------
echo R CMD REMOVE
echo ----------------------------------------------------------------
echo on
call R CMD REMOVE %1
echo off
echo.
echo ----------------------------------------------------------------
if not "%3"=="" ( 
echo R CMD check under %3
) else (
if not "%2"=="" (
echo R CMD check under %2
) else (
echo R CMD check
)
)
echo ----------------------------------------------------------------
if not "%2"=="" call Rversions %2
if not "%3"=="" call Rversions %3
echo on
call R CMD check --force-multiarch --timings %1
echo off
if not "%2"=="" call Rversions %2
echo.
echo ----------------------------------------------------------------
if "%4"=="+" (
echo R CMD check --force-multiarch --timings under %2
echo ----------------------------------------------------------------
echo on
call R CMD check --force-multiarch --timings --outdir=%2 %1
echo off
echo.
echo ----------------------------------------------------------------
)
if not "%2"=="" (
echo R CMD build under %2
) else echo R CMD build
echo ----------------------------------------------------------------
echo on
call R CMD build --compact-vignettes %1
echo off
echo.
echo ----------------------------------------------------------------
if not "%2"=="" (
echo R CMD INSTALL --build under %2
) else echo R CMD INSTALL --build 
echo ----------------------------------------------------------------
echo on
call R CMD INSTALL --force-biarch --byte-compile --with-keep.source --compact-docs --resave-data -- example --html --latex --clean --preclean --build %1
echo off
echo.
echo ----------------------------------------------------------------
if not "%2"=="" (
echo R CMD INSTALL under %2
) else echo R CMD INSTALL
echo ----------------------------------------------------------------
echo on
call R CMD INSTALL --force-biarch --byte-compile --with-keep.source --compact-docs --resave-data -- example --html --latex --clean --preclean %1
echo off
echo ----------------------------------------------------------------
if not "%3"=="" (
echo R CMD INSTALL under %3
echo ----------------------------------------------------------------
call Rversions %3
echo on
call R CMD INSTALL --force-biarch --byte-compile --with-keep.source --compact-docs --resave-data -- example --html --latex --clean --preclean %1
echo off
call Rversions %2
echo ----------------------------------------------------------------
)
echo FERTIG!!!
echo ----------------------------------------------------------------
echo on
