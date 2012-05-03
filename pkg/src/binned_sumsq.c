#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP binned_sumsq ( SEXP x, SEXP mean, SEXP bin, SEXP nbins ) {
     double *rx = REAL(x);
     double *rmean = REAL(mean);
                            
     int *ibin = INTEGER(bin);
     int nb = *INTEGER(nbins);
     int nx = length(x);
    
     SEXP res;
     PROTECT(res=allocMatrix(REALSXP, nb, 2));
     double *rres = REAL(res);
     for (int i = 0; i < 2*nb; i++){
         rres[i] = 0;
     }
                  
     for (int i = 0; i < nx; i++){
        int b = ibin[i] - 1;
        double val = rx[i] - rmean[i];
		if (-1 < b && b < nb && !ISNA(val)){
           rres[b] += 1;
           rres[b + nb] += val*val;
        }
     }  
     UNPROTECT(1);
     return res;
}
