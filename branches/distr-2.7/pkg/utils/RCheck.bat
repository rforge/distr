@echo off
if not "%2"=="" (
call R CMD check --multiarch --output=%2 --timings --install-args="--byte-compile --with-keep.source --compact-docs --resave-data --install-tests --example --html --latex --clean --preclean --compile-both --force-biarch" %1
) else (
call R CMD check --multiarch --timings --install-args="--byte-compile --with-keep.source --compact-docs --resave-data --install-tests --example --html --latex --clean --preclean --compile-both --force-biarch" %1
)
echo on
