@echo off
if not "%2"=="" (
call R CMD check --multiarch --output=%2 %1
) else (
call R CMD check  %1
)
echo on
