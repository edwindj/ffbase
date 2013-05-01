#include <R.h>
#include <Rdefines.h>
#include <R_ext/Error.h>

SEXP binned_tabulate ( SEXP x, SEXP bin, SEXP nbins, SEXP nlevels, SEXP res) {

     int *ix = INTEGER(x);
     int *ibin = INTEGER(bin);
     int nb = *INTEGER(nbins);
     int nl = *INTEGER(nlevels);
     
     int nx = length(x);
    
     int *rres = INTEGER(res);
     
     for (int i = 0; i < nx; i++){
        int b = ibin[i] - 1;
        if (-1 < b && b < nb){
			    if (NA_INTEGER == ix[i]){
			      rres[b] += 1;
			    } else {
			      rres[b + (ix[i])*nb] += 1;
			    }
		    }
     }
     return res;
}
 