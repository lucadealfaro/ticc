#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mdd.h"
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


/*
 * mdd_minmax(mgr, mdd, var_id, max)
 * computes a new relation which is identical to mdd,
 * except that only the maximum (or minimum) value of var_id is kept for each
 * valuation of the other variables.
 * 
 * The maximum is kept if max==1, 
 * the minimum is kept if max==0.
 * Other values of max lead to failure.
 *
 * Let z_0 ... z_{n-1} be the binary variables encoding var_id,
 * where z_0 represents the Most Significant Bit.
 * Let Z_{<i} denote the set of variables { z_0, ... , z_{i-1} }.
 * For maximum, we use the following algorithm:
 *
 * res = mdd
 * for i = 0 to n-1 do
 *    res = res and ( (    z_i and exists Z_{<i}  res) or 
 *                    (not z_i and forall Z_{<=i} (z_i implies not res)) )
 * return res
 *
 * For minimum, we use the following algorithm:
 *
 * res = mdd
 * for i = 0 to n-1 do
 *    res = res and ( (not z_i and exists Z_{<i}  res) or 
 *                    (    z_i and forall Z_{<=i} ((not z_i) implies not res)) )
 * return res */
mdd_t* mdd_minmax(mdd_t* mdd, int id, int max)
{
  array_t *mvar_list, *bvar_list, *bit_vars;
  mvar_type mvar;
  int i, j, n, bvar_id;
  bdd_t *literal, *term0, *term1, *res, *bvar;
  
  mdd_manager *mgr = mdd_get_manager(mdd);
  
  if (max != 0 && max != 1)
    fail("Wrong argument.");

  
  // positive is the polarity of positive literals in the max algorithm
  int positive = max;
  // negative is the polarity of negative literals in the max algorithm
  int negative = 1 - max;

  // get the global data
  mvar_list = mdd_ret_mvar_list(mgr);
  bvar_list = mdd_ret_bvar_list(mgr);

  // get the characteristics of the mdd variable "id" 
  mvar = array_fetch(mvar_type, mvar_list, id);
  n = mvar.encode_length;
  // initialize the result variable
  res = mdd;

  bit_vars = array_alloc(bdd_t *, n);
  // reverse mvar.bvars into bit_vars, 
  // so that later we can use array_remove_last 
  // to discard more significant variables from the bit_vars
  for (i = 0, j = n-1; i<n ;i++, j--) {
    bvar_id = mdd_ret_bvar_id(&mvar, i);
    bvar = bdd_get_variable(mgr, bvar_id);
    array_insert(bdd_t *, bit_vars, j, bvar);
  }

  for (i = 0; i<n; i++) {

    // get the i-th most significant bit z_i
    bvar_id = mdd_ret_bvar_id(&mvar, i);
    // get the literal "z_i = 1"
    literal = bdd_get_variable(mgr, bvar_id);

    // build the term for z_i = 0 (max)
    term0 = bdd_or(literal, res, negative, 0);
    term0 = bdd_consensus(term0, bit_vars);
    term0 = bdd_and(literal, term0, negative, 1);

    array_remove_last(bit_vars);

    // build the term for z_i = 1 (max)
    term1 = bdd_smooth(res, bit_vars);
    term1 = bdd_and(literal, term1, positive, 1);

    // put together both terms
    term0 = bdd_or(term0, term1, 1, 1);
    // update the result
    res = bdd_and(res, term0, 1, 1);
  }
  array_free(bit_vars);

  return res;
}


/*
 * mdd_incr(mgr, mdd, var_id)
 *
 * Computes a new relation which is identical to mdd,
 * except that the value of var_id is incremented by one 
 * in each tuple belonging to the relation where 
 * the value of var_id is not already maxed out.
 * 
 * Let z_0 ... z_{n-1} be the binary variables encoding var_id,
 * where z_0 represents the Most Significant Bit.
 * Let Z_{<i} denote the set of variables { z_0, ... , z_{i-1} }.
 * We use the following algorithm:
 *
 * res = false
 * for i = n-1 to 0 do
 *    old_prefix =     z_{n-1} and     z_{n-2} and ... and     z_{i+1}
 *    new_prefix = not z_{n-1} and not z_{n-2} and ... and not z_{i+1}
 *    res = res or ( new_prefix and z_i and exists Z_{>=i} (mdd and old_prefix and not z_i) )
 * return res
 */
mdd_t* mdd_incr(mdd_t* mdd, int id)
{
  array_t *mvar_list, *bvar_list, *smooth_vars, *max_val_array;
  mvar_type mvar;
  int i, n, max_val, bvar_id;
  bdd_t *literal, *max_val_literal, *term, *old_prefix, *new_prefix, *res;
  bdd_t *mdd_maxed_out, *mdd_not_maxed_out;

  mdd_manager *mgr = mdd_get_manager(mdd);

  // get the global data
  mvar_list = mdd_ret_mvar_list(mgr);
  bvar_list = mdd_ret_bvar_list(mgr);

  // get the characteristics of the mdd variable "id" 
  mvar = array_fetch(mvar_type, mvar_list, id);
  n = mvar.encode_length;
  max_val = mvar.values -1;

  // initialize the result variable
  res = bdd_zero(mgr);
  old_prefix = bdd_one(mgr);
  new_prefix = bdd_one(mgr);

  // isolate term with maximum value from the rest
  max_val_array = array_alloc(int, 1);
  array_insert(int, max_val_array, 0, max_val);
  max_val_literal = mdd_literal(mgr, id, max_val_array);
  // it holds mdd = mdd_maxed_out OR mdd_not_maxed_out
  mdd_maxed_out     = mdd_and(mdd, max_val_literal, 1, 1);
  mdd_not_maxed_out = mdd_and(mdd, max_val_literal, 1, 0);

  smooth_vars = array_alloc(bdd_t *, 0);

  // from the least to the most significant
  for (i=n-1; i>=0 ;i--) {

    // get the i-th most significant bit z_i
    bvar_id = mdd_ret_bvar_id(&mvar, i);
    // get the literal "z_i = 1"
    literal = bdd_get_variable(mgr, bvar_id);
    // add z_i to the set of variables to be smoothed
    array_insert_last(bdd_t *, smooth_vars, literal);

    term = bdd_and(mdd_not_maxed_out, old_prefix, 1, 1);
    term = bdd_and(term, literal, 1, 0);
    term = bdd_smooth(term, smooth_vars);
    term = bdd_and(term, new_prefix, 1, 1);
    term = bdd_and(term, literal, 1, 1);
    res = bdd_or(res, term, 1, 1);

    // post-update updates
    // prolong conjunction of positive literals
    old_prefix = bdd_and(old_prefix, literal, 1, 1);
    // prolong conjunction of negative literals
    new_prefix = bdd_and(new_prefix, literal, 1, 0);
  }
  array_free(smooth_vars);
  array_free(max_val_array);
  
  // add the extra term containing the tuples where
  // the variable is already maxed out.
  res = bdd_or(res, mdd_maxed_out, 1, 1);

  return res;
}



int mdd_get_unique_value(mdd_manager* mgr, mdd_t* mdd, int id)
{
  int i, n, bvar_id, res = 0;
  array_t *mvar_list, *bvar_list, *smooth_var;
  mvar_type mvar;
  bdd_t *literal, *true_factor;

  // get the global data
  mvar_list = mdd_ret_mvar_list(mgr);
  bvar_list = mdd_ret_bvar_list(mgr);
  smooth_var = array_alloc(bdd_t *, 1);

  // get the characteristics of the mdd variable "id" 
  mvar = array_fetch(mvar_type, mvar_list, id);
  n = mvar.encode_length;

  for (i = 0; i<n; i++) {
    // get the i-th most significant bit z_i
    bvar_id = mdd_ret_bvar_id(&mvar, i);
    // get the literal "z_i = 1"
    literal = bdd_get_variable(mgr, bvar_id);
    // compute the cofactor w.r.t. z_i
    true_factor = bdd_and(mdd, literal, 1, 1);
    array_insert(bdd_t *, smooth_var, 0, literal);
    true_factor = bdd_smooth(true_factor, smooth_var);

    // if the z_i cofactor is not equal to false,
    // we set the i-th bit to 1.
    // possible optimization: recur on true_factor, 
    // rather than always start from mdd
    if (!bdd_is_tautology(true_factor, 0))
      res |= 1 << (n - i - 1);
  } 
  return res;
}

/*
array_t *mdd_get_values(mdd_manager* mgr, mdd_t* mdd, int id)
{
  array_t *mvar_list, *bvar_list;
  array_t *res = array_alloc(int, 0);

  // get the global data
  mvar_list = mdd_ret_mvar_list(mgr);
  bvar_list = mdd_ret_bvar_list(mgr);

  // get the characteristics of the mdd variable "id" 
  mvar = array_fetch(mvar_type, mvar_list, id);
  n = mvar.encode_length;

  for (i = 0; i<n; i++) {
    // get the i-th most significant bit z_i
    bvar_id = mdd_ret_bvar_id(&mvar, i);
    // get the literal "z_i = 1"
    literal = bdd_get_variable(mgr, bvar_id);
  }  
}
*/
