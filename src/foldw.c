#include <Rinternals.h>

/* takes an index from st_is_within_distance
   (list of integer vectors) and returns
   a single integer vector of indices which
   avoids to change indices in the sequence
   as long as they are in the candidate set.
*/
SEXP foldw(SEXP sIdx) {
    size_t n = XLENGTH(sIdx), i = 0;
    SEXP res = Rf_allocVector(INTSXP, n);
    int *dst = INTEGER(res), last = -1;
    if (n < 1)
	return res;
    PROTECT(res);
    /* skip empty entries */
    while (i < n && XLENGTH(VECTOR_ELT(sIdx, i)) == 0) {
	*(dst++) = NA_INTEGER;
	i++;
    }
    /* prime at least one - if it's multiple,
       pick the first one (for no good reason, we have
       to start soemwhere) */
    if (i == 0)
	last = *(dst++) = INTEGER(VECTOR_ELT(sIdx, i++))[0];
    while (i < n) {
	SEXP v = VECTOR_ELT(sIdx, i++);
	size_t cl = XLENGTH(v);
	/* only one? easy ... */
	if (cl == 1) {
	    last = *(dst++) = INTEGER(v)[0];
	    continue;
	}
	/* no match? Don't udpate last */
	if (cl == 0) {
	    *(dst++) = NA_INTEGER;
	    continue;
	}
	int found = 0;
	int *c = INTEGER(v);
	size_t j = 0;
	while (j < cl) {
	    if (c[j] == last)
		found = 1;
	    j++;
	}
	/* previous is one of the close ones,
	   keep it */
	if (found) {
	    *(dst++) = last;
	    continue;
	}
	/* previous is not part of it ...
	   this is sort of bad, because it
	   means we have multiple entries, but
	   none is the last one, so no good way
	   to pick ... */
	last = *(dst++) = c[0];
    }
    UNPROTECT(1);
    return res;
}
