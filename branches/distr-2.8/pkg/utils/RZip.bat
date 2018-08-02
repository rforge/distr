@echo off
mkdir tmpInstall00
call R CMD INSTALL --build --library=tmpInstall00 --byte-compile --with-keep.source --compact-docs --resave-data --install-tests --example --html --latex --clean --preclean --compile-both %1
rmdir /S/Q tmpInstall00
echo on
