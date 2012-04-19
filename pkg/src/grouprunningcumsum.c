/*
 *  Groups the input integer x into several groups if the running cumulative sum increases a certain maximum number
 *  Jan Wijffels jwijffels [at] bnosac.be 
 *
 *	Requires 3 parameters: 
 *		- an input integer string, 
 *		- the length of the integer string and 
 *		- the maximum cumulative size before splitting is done
 *
 */
#include <R.h>

void grouprunningcumsum(Sint *x, Sint *l, Sint *max) {   
		int i, size=*l, xinit=*x, maxsize, runningcumul=0;
		maxsize = max[0];
    for (i = 0; i < size; i++){
 	  	if (i == 0){
 	  		runningcumul = x[i];
 	  		x[i] = 1;
 	  	} 	  		
			else{
				runningcumul = runningcumul + x[i];
				if (runningcumul > maxsize){
					runningcumul = x[i];
					x[i] = x[i-1]+1;					
				}else{
					x[i] = x[i-1];
				}
			}
    }
}
