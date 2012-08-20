#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP bySumsq ( SEXP x, SEXP mean, SEXP by, SEXP nbins, SEXP weight) {
     double *rx = REAL(x);
     double *rmean = REAL(mean);
     double *w = REAL(weight);
                            
     int *ibin = INTEGER(by);
     int nb = *INTEGER(nbins);
     int nx = length(x);
    
     SEXP res;
     PROTECT(res=allocMatrix(REALSXP, nb, 2));
     double *rres = REAL(res);
     for (int i = 0; i < 3*nb; i++){
         rres[i] = 0;
     }
                  
     for (int i = 0; i < nx; i++){
        int b = ibin[i] - 1;
        double val = rx[i] - rmean[i];
		    if (-1 < b && b < nb){
           if (!ISNA(val)){
             rres[b + 2*nb] += w[i];
           } else {
             rres[b] += w[i];
             rres[b + nb] += w[i]*val*val;
           }
        }
     }
     UNPROTECT(1);
     return res;
}
