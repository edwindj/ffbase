#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP binned_sumsq ( SEXP x, SEXP mean, SEXP bin, SEXP nbins, SEXP res) {
     double *rx = REAL(x);
     double *rmean = REAL(mean);
                            
     int *ibin = INTEGER(bin);
     const int nb = *INTEGER(nbins);
     const int nx = length(x);
    
     double *rres = REAL(res);
     for (int i = 0; i < nx; i++){
        int b = ibin[i] - 1;
        double val = rx[i] - rmean[i];
		    if (-1 < b && b < nb){
  		    if (!ISNA(val)){
             rres[b] += 1;
             rres[b + nb] += val*val;
          } else {
            rres[b + 2*nb] += 1;
  		    }
        }
     }
     return res;
}
