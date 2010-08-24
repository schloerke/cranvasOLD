#include <math.h>

double find_yi(double x0, double yi[], int *p) {
	int i;
  if (x0 <= 1.0) {
    return yi[0];
  } else {
    if (x0 >= *p) {
  		return yi[*p - 1];
    } else {
  		i = floor(x0);
	    return (yi[i] - yi[i-1]) * (x0 - i) + yi[i-1];
    }
  }
}


void pcp_find_y(int *n, int *p, double *xb, double *y, double *tmp0)
{
  int i, j;
  double yi[*p];
  
  for (i = 0; i <= *n-1; i++) {
  	for (j = 0; j <= *p-1; j++) {
	    yi[j] = y[i * *p + j];
  	}
    tmp0[i] = find_yi(xb[0], yi, p);
    tmp0[i + *n] = find_yi(xb[1], yi, p);
  }
}

/*

            for (i in 1:n) {
                tmp0[i, ] = c(f(xb[1], y[i, ]), f(xb[2], y[i, 
                  ]))
            }
            # given x, calculate y
            f = function(x0, y) {
		if (x0 <= 1) 
		    return(y[1])
		    else {
			if (x0 >= p) 
			    return(y[p])
			    else {
				i = max(which(1:p <= x0))
				return((y[i + 1] - y[i]) * (x0 - i) + y[i])
	    }
	    }
	    }
*/

/*
system('R CMD SHLIB pcp_find_y.c')
dyn.load('pcp_find_y.so')
.C('pcp_find_y', n=10L, p=3L, xb=c(1.5,2.5), y=runif(30), tmp0=rep(0,20))
dyn.unload('pch_find_y.so')
*/
