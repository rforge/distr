@echo off
set _R_CHECK_LENGTH_1_CONDITION_=true
if not "%2"=="" (
call R CMD check --as-cran --output=%2  %1
) else (
call R CMD check --as-cran %1
)
set _R_CHECK_LENGTH_1_CONDITION_=
echo on
