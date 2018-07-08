@echo off
call R CMD build --compact-vignettes="gs+qpdf" --resave-data %1
echo on
