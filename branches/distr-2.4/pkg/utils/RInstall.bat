@echo off
call R CMD INSTALL --byte-compile --with-keep.source --compact-docs --resave-data --install-tests --example --html --latex --clean --preclean --compile-both %1
echo on