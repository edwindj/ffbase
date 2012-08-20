#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP bySum ( SEXP x, SEXP by, SEXP nbins, SEXP weight) {

     double *rx = REAL(x);
     int *ibin = INTEGER(by);
     int nb = *INTEGER(nbins);
     int nx = length(x);
     double *w = REAL(weight);
    
     SEXP res;
     PROTECT(res=allocMatrix(REALSXP, nb, 3));
     double *rres = REAL(res);
    
	   for (int i = 0; i < 3*nb; i++){
         rres[i] = 0;
     }
                  
     for (int i = 0; i < nx; i++){
        int b = ibin[i] - 1;
        if (-1 < b && b < nb){
           if (ISNA(rx[i])){
             rres[b + 2*nb] += w[i];
           } else {
             rres[b] += w[i];
             rres[b + nb] += w[i] * rx[i];
           }
        }
     }
     UNPROTECT(1);
     return res;
}
