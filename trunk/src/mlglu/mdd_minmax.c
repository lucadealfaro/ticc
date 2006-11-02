#include <stdio.h>
#include <math.h>
#include "mdd.h"
#include "array.h"
#include "mlglu_util.h"

/*
 * MDD Package
 *
 * $Id: mdd_minmax.c,v 1.4 2006/01/28 05:31:32 vishwa Exp $
 *
 * Copyright 2006 by the Regents of the University of California.
 *
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software is hereby granted, provided that the above copyright
 * notice and this permission notice appear in all copies.  This software
 * is made available as is, with no warranties.
 */

	
mdd_t* mdd_max_on_mdd(mdd_manager* mgr, mdd_t* mdd, int id)
{
  int i, nvals;
  mdd_t *s, *t, *not_s, *literal, *bigger_terms;
  array_t* cases;

  nvals = getNVals(mgr, id);
  cases = array_alloc(mdd_t *, nvals-1);

  bigger_terms = mdd_one(mgr);

  for (i=nvals-1; i >= 0; i--) {

    literal = UtilGetLiteral(mgr, id, i);
      
    s = mdd_cofactor(mgr, mdd, literal);
    t = mdd_and(s, bigger_terms, 1, 1);
    array_insert(mdd_t *, cases, i, t);
    not_s = mdd_not(s);
    bigger_terms = mdd_and(bigger_terms, not_s, 1, 1);
  }

  s = mdd_case(mgr, id, cases);
  array_free(cases);

  return s;
}


mdd_t* mdd_max_on_bits(mdd_manager* mgr, mdd_t* mdd, int id)
{
  array_t *mvar_list, *bvar_list, *bit_vars;
  mvar_type mvar;
  int i, j, n, bvar_id;
  bdd_t *literal, *term0, *term1, *res, *bvar;

  // get the global data
  mvar_list = mdd_ret_mvar_list(mgr);
  bvar_list = mdd_ret_bvar_list(mgr);

  // get the characteristics of the mdd variable id 
  mvar = array_fetch(mvar_type, mvar_list, id);
  n = mvar.encode_length;
  // initialize the result variable
  res = mdd;

  bit_vars = array_alloc(bdd_t *, n);
  // reverse mvar.bvars into bit_vars
  for (i = 0, j = n-1; i<n ;i++, j--) {
    bvar_id = mdd_ret_bvar_id(&mvar, i);
    bvar = bdd_get_variable(mgr, bvar_id);
    array_insert(bdd_t *, bit_vars, j, bvar);
  }

  for (i = 0; i<n; i++) {

    printf("i = %d\n", i);

    bvar_id = mdd_ret_bvar_id(&mvar, i);
    literal = bdd_get_variable(mgr, bvar_id);

    // build the term for z_i = 0
    term0 = bdd_or(literal, res, 0, 0);
    term0 = bdd_consensus(term0, bit_vars);
    term0 = bdd_and(literal, term0, 0, 1);

    array_remove_last(bit_vars);

    // build the term for z_i = 1
    term1 = bdd_smooth(res, bit_vars);
    term1 = bdd_and(literal, term1, 1, 1);

    // put together both terms
    term0 = bdd_or(term0, term1, 1, 1);
    res = bdd_and(res, term0, 1, 1);
  }
  array_free(bit_vars);

  return res;
}
