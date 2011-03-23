/************************************************
 *   CITAN - CITation ANalysis toolpack for R   *
 *   Marek Gagolewski <gagolews@ibspan.waw.pl>  *
 ************************************************/


#ifndef __pareto2_h
#define __pareto2_h


/* #define CITAN_DEBUG */


#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdio.h>




void pareto2_phirsch(double* x, int* m, double* n, double* k, double* s);
void pareto2_dhirsch(double* x, int* m, double* n, double* k, double* s);



#endif
