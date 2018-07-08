@echo off
echo.
if not "%1"=="" (
   el cmd /c Rversions %1 %2
) else (
   Rversions  
)
