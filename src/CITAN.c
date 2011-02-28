/************************************************
 *   CITAN - CITation ANalysis toolpack for R   *
 *   Marek Gagolewski <gagolews@ibspan.waw.pl>  *
 ************************************************/


#include "impact_functions.h"


static const R_CMethodDef cMethods[] = {
	{"index_h", (DL_FUNC)&index_h, 3},
	{"index_h_log", (DL_FUNC)&index_h_log, 3},
	{"index_g", (DL_FUNC)&index_g, 3},
	{"Sstat2", (DL_FUNC)&Sstat2, 3},
	{"index_rp_finite", (DL_FUNC)&index_rp_finite, 4},
	{"index_lp_finite", (DL_FUNC)&index_lp_finite, 5},
	{"index_rp_infinite", (DL_FUNC)&index_rp_infinite, 3},
	{"index_lp_infinite", (DL_FUNC)&index_lp_infinite, 3},
	{NULL, NULL, 0}
};


void R_init_CITAN(DllInfo *dll)
{
	R_registerRoutines(dll, cMethods, NULL, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);

#ifdef CITAN_DEBUG
	fprintf(stderr, "DEBUG: Dynamic library CITAN loaded.\n");
#endif
}

