@echo off

echo Removing building information...
rm -rf pkg/man
rm -f *.tar.gz

echo Generate documentation...
R -f roxygen.R

R CMD build pkg
R CMD check ffbase_0.2.tar.gz