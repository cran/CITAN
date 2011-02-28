/************************************************
 *   CITAN - CITation ANalysis toolpack for R   *
 *   Marek Gagolewski <gagolews@ibspan.waw.pl>  *
 ************************************************/


#ifndef __impact_functions_h
#define __impact_functions_h


/* #define CITAN_DEBUG */


#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdio.h>




void index_h(double* x, int* n, double* out);


int __index_h_log(double* x, int n);

void index_h_log(double* x, int* n, double* out);

void index_g(double* x, int* n, double* out);

void Sstat2(double* x, int* n, double* out);

void index_rp_finite(double* x, int* n, double *p, double* out);

void index_rp_infinite(double* x, int* n, double* out);



int __index_lp_finite_testContains(double uk, double vk, double p, double ui, double vi, double uj, double vj);

void __index_lp_finite_getAB(double p, double ui, double vi, double uj, double vj, double* a2p, double* b2p);

void index_lp_finite(double* x, int* n, double *p, int* s, double* out);

void index_lp_infinite(double* x, int* n, double* out);


#endif
