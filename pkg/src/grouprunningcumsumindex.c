/*
 *  Indicates as 1 or 0 whether the running cumulative sum increases a certain maximum number or 2 that the value is also bigger that the max
 *  Jan Wijffels jwijffels [at] bnosac.be 
 *
 *	Requires 3 parameters: 
 *		- an input integer string, 
 *		- the length of the integer string and 
 *		- the maximum cumulative size before splitting is done
 *		- a startoff currentcumul
 *
 */
#include <R.h>

void grouprunningcumsumindex(Sint *x, Sint *l, Sint *max, Sint *currentcumul) {   
		int i, size=*l, xinit=*x, maxsize, runningcumul;
		maxsize = max[0];
		runningcumul = currentcumul[0];
    for (i = 0; i < size; i++){
 	  	if (x[i] >= maxsize){
 	  		x[i] = 2;
 	  		runningcumul = 0;
 	  	}else{
	 	 	  	runningcumul = runningcumul + x[i];
	 	 	  	if (runningcumul >= maxsize){
						runningcumul = 0;
						x[i] = 1;
					}else{
						x[i] = 0;
					}
 	  	}			
    }
    currentcumul[0] = runningcumul;
}
