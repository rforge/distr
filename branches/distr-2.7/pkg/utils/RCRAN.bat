@echo off
if not "%2"=="" (
call R CMD check --as-cran --output=%2  %1
) else (
call R CMD check --as-cran %1
)
echo on
