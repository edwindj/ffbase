R -f roxygen.R
R CMD build pkg
R CMD check ffExtras_0.2.tar.gz

REM pause