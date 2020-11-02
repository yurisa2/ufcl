void
ufcl(double *x, int *nr_x, int *nc, double *p, int *nr_p, double *w,
     double *f, int *dist, int *itermax, double *reltol, int *verbose,
     double *rate_par,
     double *u, double *ermin, int *iter)
{
    double exponent = 1 / (*f - 1);
    double old_value, new_value;

    int ix;
    double lrate;

    cmeans_setup(*nr_x, *nr_p, 0);

    /* Need some starting values ... */
    cmeans_dissimilarities(x, p, *nr_x, *nc, *nr_p, *dist, d);
    cmeans_memberships(d, *nr_x, *nr_p, exponent, u);
    old_value = new_value = cmeans_error_fn(u, d, w, *nr_x, *nr_p, *f);

    *iter = 0;
    while((*iter)++ < *itermax) {
	/* Turns out that sampling the objects is a bad idea ... */
	lrate = *rate_par * (1 - (double) *iter / *itermax);
	for(ix = 0; ix < *nr_x; ix++) {
	    ufcl_dissimilarities(x, p, *nr_x, *nc, *nr_p, *dist, ix, d);
	    ufcl_memberships(d, *nr_x, *nr_p, exponent, ix, u);
	    ufcl_prototypes(x, u, w, *nr_x, *nc, *nr_p, *f, *dist,
			    lrate, ix, p);
	}
	new_value = cmeans_error_fn(u, d, w, *nr_x, *nr_p, *f);
	if(fabs(old_value - new_value) < *reltol * (old_value + *reltol)) {
	    if(*verbose)
		Rprintf("Iteration: %3d converged, Error: %13.10f\n",
			*iter, new_value);
	    break;
	}
	else {
	    if(*verbose) {
		*ermin = cmeans_error_fn(u, d, w, *nr_x, *nr_p, *f);
		Rprintf("Iteration: %3d, Error: %13.10f\n",
			*iter, new_value);
	    }
	    old_value = new_value;
	}
    }

    *ermin = new_value;
}	
