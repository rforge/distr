echo off
REM
REM ***rebuild*** makes all my packages...
REM
REM  SYNTAX: rebuild [R-ver] [Rver2] [+]
REM   * performs R CMD check, R CMD build, R CMD build --binary, R CMD INSTALL to packages
REM     startupmsg, distr, distrEx, distrSim, distrTEst, distrDoc (in this order)
REM   with R-ver    the R-version under which the packages are to be made; i.e. Rversion is changed
REM          R-ver2  the R-version under which the packages are to be checked,
REM                        i.e. Rversion is changed before and after check
REM                        if given installs to R-ver & R-ver2  
REM          +            packages are checked agaist R-ver and R-ver2 
REM                        (else just against R-ver2 if given or R-ver if given or preset R-version)
REM           all args are positional, i.e. they only apply if all preceding args are given
REM 
echo.
echo ##########################################
echo This is rebuild --- making all packages....
echo ##########################################
echo.
echo.
echo.
call pmake startupmsg >pmake-startupmsg.log
call pmake distr %1 %2 %3 >pmake-distr.log
call pmake distrEx %1 %2 %3 >pmake-distrEx.log
call pmake distrTeach %1 %2 %3 >pmake-distrTeach.log
call pmake distrSim %1 %2 %3 >pmake-distrSim.log
call pmake distrTEst %1 %2 %3 >pmake-distrTEst.log
call pmakeDoc %1 %2 %3 >pmake-distrDoc.log
