@echo off

echo Removing building information...
rm -rf pkg/man
rm -rf pkg/inst
rm -f *.tar.gz

echo Generate documentation...
R -f roxygen.R
rm -rf pkg/inst

md output
cd output
R CMD build ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1