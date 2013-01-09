@echo off
call R CMD build --compact-vignettes --resave-data %1
echo on