@echo off
if not "%2"=="" (
call R CMD check --as-cran --outdir=%2  %1
) else (
call R CMD check --as-cran %1
)
echo on
