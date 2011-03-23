/************************************************
 *   CITAN - CITation ANalysis toolpack for R   *
 *   Marek Gagolewski <gagolews@ibspan.waw.pl>  *
 ************************************************/


#include "pareto2.h"


void pareto2_phirsch(double* x, int* m, double* n, double* k, double* s)
{
	int M = *m;
	double N = *n;
	double K = *k;
	double S = *s;
	for (int i=0; i<M; ++i)
	{
		if      (x[i]>N-1e-9)     x[i] = 1.0;
		else if (x[i]<0.0)        x[i] = 0.0;
		else
		{
			double floorx = floor(x[i]);
			x[i] = pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0);
		}
	}
}



void pareto2_dhirsch(double* x, int* m, double* n, double* k, double* s)
{
	int M = *m;
	double N = *n;
	double K = *k;
	double S = *s;
	for (int i=0; i<M; ++i)
	{
		if (x[i] > 1e-9 && x[i] < N-1e-9)
		{
			double floorx = floor(x[i]+1e-9);
			x[i] = pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0)
			      -pbeta(1.0-pow(S/(S+floorx),K),
				N-floorx+1.0,
				floorx,
				1, 0);
		}
		else if (x[i] > N+1e-9 || x[i] < 0.0) x[i] = 0.0;
		else if (x[i] > N-1e-9)
		{
			double floorx = floor(x[i]-1e-9);
			x[i] = 1.0-pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0);
		}
		else /* if (x[i] < 1e-9) */
		{
			double floorx = floor(x[i]+1e-9);
			x[i] = pbeta(1.0-pow(S/(S+floorx+1.0),K),
				N-floorx,
				floorx+1.0,
				1, 0);
		}
	}
}



