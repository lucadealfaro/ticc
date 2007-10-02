/******************************************************************************

  PackageName [bdd]

  Synopsis    [Package-independent BDD interface functions.]

  Description []

  SeeAlso     []

  Author      [Thomas R. Shiple. Modified by Rajeev K. Ranjan.]

  Copyright   [Copyright (c) 1994-1996 The Regents of the Univ. of California.
  All rights reserved.

  Permission is hereby granted, without written agreement and without license
  or royalty fees, to use, copy, modify, and distribute this software and its
  documentation for any purpose, provided that the above copyright notice and
  the following two paragraphs appear in all copies of this software.

  IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
  DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
  OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
  CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
  "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO PROVIDE
  MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.]

  Revision    [$Id: bdd.h,v 1.3 2006/01/28 05:31:32 vishwa Exp $]

******************************************************************************/

#ifndef _BDD_H
#define _BDD_H
#include "util.h"  
#include "array.h"
#include "st.h"
#include "var_set.h"
#include "avl.h"
#ifndef	EPD_MAX_BIN
#include "epd.h"
#endif

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/
typedef int boolean;
typedef void bdd_manager;
typedef unsigned int bdd_variableId; /* the id of the variable in a bdd node */
typedef void bdd_mgr_init; /* Typecasting to void to avoid error messages */
typedef int bdd_literal;	        /* integers in the set { 0, 1, 2 } */
typedef struct bdd_t bdd_t;
typedef void bdd_node;
typedef void bdd_gen;
typedef struct bdd_external_hooks_struct bdd_external_hooks;
typedef void bdd_block;
typedef double BDD_VALUE_TYPE;

/*---------------------------------------------------------------------------*/
/* Enumerated type declarations                                              */
/*---------------------------------------------------------------------------*/

/*
 * Dynamic reordering.
 */
typedef enum {
  BDD_REORDER_SIFT,
  BDD_REORDER_WINDOW,
  BDD_REORDER_SAME,
  BDD_REORDER_RANDOM,
  BDD_REORDER_RANDOM_PIVOT,
  BDD_REORDER_SIFT_CONVERGE,
  BDD_REORDER_SYMM_SIFT,
  BDD_REORDER_SYMM_SIFT_CONV,
  BDD_REORDER_LINEAR,
  BDD_REORDER_LINEAR_CONVERGE,
  BDD_REORDER_EXACT,
  BDD_REORDER_WINDOW2,
  BDD_REORDER_WINDOW3,
  BDD_REORDER_WINDOW4,
  BDD_REORDER_WINDOW2_CONV,
  BDD_REORDER_WINDOW3_CONV,
  BDD_REORDER_WINDOW4_CONV,
  BDD_REORDER_GROUP_SIFT,
  BDD_REORDER_GROUP_SIFT_CONV,
  BDD_REORDER_ANNEALING,
  BDD_REORDER_GENETIC,
  BDD_REORDER_LAZY_SIFT,
  BDD_REORDER_NONE
} bdd_reorder_type_t;

typedef enum {
  CMU,
  CAL,
  CUDD
} bdd_package_type_t;

typedef enum {
    bdd_EMPTY,
    bdd_NONEMPTY
} bdd_gen_status;

typedef enum {
    BDD_PRE_GC_HOOK,
    BDD_POST_GC_HOOK,
    BDD_PRE_REORDERING_HOOK,
    BDD_POST_REORDERING_HOOK
} bdd_hook_type_t;

typedef enum {
    BDD_OVER_APPROX,
    BDD_UNDER_APPROX
} bdd_approx_dir_t;

typedef enum {
    BDD_CONJUNCTS,
    BDD_DISJUNCTS
} bdd_partition_type_t;

typedef enum {
  BDD_REORDER_VERBOSITY_DEFAULT,
  BDD_REORDER_NO_VERBOSITY,
  BDD_REORDER_VERBOSITY
}bdd_reorder_verbosity_t;

typedef enum {
  BDD_APPROX_HB,
  BDD_APPROX_SP,
  BDD_APPROX_COMP,
  BDD_APPROX_UA,
  BDD_APPROX_RUA,
  BDD_APPROX_BIASED_RUA
} bdd_approx_type_t;


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/
struct bdd_external_hooks_struct {
    char *network;
    char *mdd;
    char *undef1;
};


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/
/*
** It is assumed that dynamic reordering will not occur while there are open
** generators.  It is the user's responsibility to make sure dynamic
** reordering doesn't occur. As long as new nodes are not created during
** generation, and you don't explicitly call dynamic reordering, you should be
** okay. 
*/

/*
 *    foreach_bdd_cube(fn, gen, cube)
 *    bdd_t *fn;
 *    bdd_gen *gen;
 *    array_t *cube;	- return
 *
 *    foreach_bdd_cube(fn, gen, cube) {
 *        ...
 *    }
 */
#define foreach_bdd_cube(fn, gen, cube)\
  for((gen) = bdd_first_cube(fn, &cube);\
      (bdd_gen_read_status(gen) != bdd_EMPTY) ? TRUE: bdd_gen_free(gen);\
      (void) bdd_next_cube(gen, &cube))

/*
 *    foreach_bdd_node(fn, gen, node)
 *    bdd_t *fn;
 *    bdd_gen *gen;
 *    bdd_node *node;	- return
 */
#define foreach_bdd_node(fn, gen, node)\
  for((gen) = bdd_first_node(fn, &node);\
      (bdd_gen_read_status(gen) != bdd_EMPTY) ? TRUE: bdd_gen_free(gen);\
      (void) bdd_next_node(gen, &node))


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/


/**AutomaticEnd***************************************************************/

EXTERN bdd_package_type_t bdd_get_package_name ARGS((void));

/*
 * BDD Manager Allocation And Destruction
 */
EXTERN bdd_manager *bdd_start ARGS((int));
EXTERN void bdd_end ARGS((bdd_manager *));

/*
 * BDD variable management
 */
EXTERN bdd_t *bdd_create_variable ARGS((bdd_manager *));		
EXTERN bdd_t *bdd_create_variable_after ARGS((bdd_manager *, bdd_variableId));
EXTERN bdd_t *bdd_get_variable ARGS((bdd_manager *, bdd_variableId));	
EXTERN bdd_t *bdd_create_variable_after ARGS((bdd_manager *, bdd_variableId));
EXTERN bdd_t * bdd_var_with_index ARGS((bdd_manager *manager, int index));
EXTERN bdd_node *bdd_add_ith_var ARGS((bdd_manager *mgr, int i));


/*
 * BDD Formula Management
 */
EXTERN bdd_t *bdd_dup ARGS((bdd_t *));
EXTERN void bdd_free ARGS((bdd_t *));

/* 
 * Operations on BDD Formulas
 */
EXTERN bdd_t *bdd_and ARGS((bdd_t *, bdd_t *, boolean, boolean));
EXTERN bdd_t *bdd_and_with_limit ARGS((bdd_t *, bdd_t *, boolean, boolean, unsigned int));
EXTERN bdd_t *bdd_and_array ARGS((bdd_t *, array_t *, boolean, boolean));
EXTERN bdd_t *bdd_and_smooth ARGS((bdd_t *, bdd_t *, array_t *));
EXTERN bdd_t *bdd_and_smooth_with_limit ARGS((bdd_t *, bdd_t *, array_t *, unsigned int));
EXTERN bdd_t *bdd_and_smooth_with_cube ARGS((bdd_t *, bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_clipping_and_smooth ARGS((bdd_t *, bdd_t *, array_t *, int , int ));
EXTERN bdd_t *bdd_xor_smooth ARGS((bdd_t *, bdd_t *, array_t *));
EXTERN bdd_t *bdd_between ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_cofactor ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_cofactor_array ARGS((bdd_t *, array_t *));
EXTERN bdd_t *bdd_var_cofactor ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_compose ARGS((bdd_t *, bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_vector_compose ARGS((bdd_t *, array_t *, array_t *));
EXTERN bdd_t *bdd_consensus ARGS((bdd_t *, array_t *));
EXTERN bdd_t *bdd_cproject ARGS((bdd_t *, array_t *));
EXTERN bdd_t *bdd_else ARGS((bdd_t *));
EXTERN bdd_t *bdd_ite ARGS((bdd_t *, bdd_t *, bdd_t *, boolean, boolean, boolean));
EXTERN bdd_t *bdd_minimize ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_minimize_array ARGS((bdd_t *, array_t *));
EXTERN bdd_t *bdd_compact ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_squeeze ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_not ARGS((bdd_t *));
EXTERN bdd_t *bdd_one ARGS((bdd_manager *));
EXTERN bdd_t *bdd_or ARGS((bdd_t *, bdd_t *, boolean, boolean));
EXTERN bdd_t *bdd_smooth ARGS((bdd_t *, array_t *));
EXTERN bdd_t *bdd_smooth_with_cube ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_substitute ARGS((bdd_t *, array_t *, array_t *));
EXTERN bdd_t *bdd_substitute_with_permut ARGS((bdd_t *, int *));
EXTERN array_t *bdd_substitute_array ARGS((array_t *, array_t *, array_t *));
EXTERN array_t *bdd_substitute_array_with_permut ARGS((array_t *, int *));
EXTERN void *bdd_pointer ARGS((bdd_t *));
EXTERN bdd_t *bdd_then ARGS((bdd_t *));
EXTERN bdd_t *bdd_top_var ARGS((bdd_t *));
EXTERN bdd_t *bdd_xnor ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_xor ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_zero ARGS((bdd_manager *));
EXTERN bdd_t *bdd_multiway_and ARGS((bdd_manager *, array_t *));
EXTERN bdd_t *bdd_multiway_or ARGS((bdd_manager *, array_t *));
EXTERN bdd_t *bdd_multiway_xor ARGS((bdd_manager *, array_t *));
EXTERN array_t * bdd_pairwise_or ARGS((bdd_manager *manager, array_t *bddArray1, array_t *bddArray2));
EXTERN array_t * bdd_pairwise_and ARGS((bdd_manager *manager, array_t *bddArray1, array_t *bddArray2));
EXTERN array_t * bdd_pairwise_xor ARGS((bdd_manager *manager, array_t *bddArray1, array_t *bddArray2));
EXTERN bdd_t *bdd_approx_hb ARGS((bdd_t *, bdd_approx_dir_t , int , int ));
EXTERN bdd_t *bdd_approx_sp ARGS((bdd_t *, bdd_approx_dir_t , int , int , int ));
EXTERN bdd_t *bdd_approx_ua ARGS((bdd_t *, bdd_approx_dir_t , int , int , int , double ));
EXTERN bdd_t *bdd_approx_remap_ua ARGS((bdd_t *, bdd_approx_dir_t , int , int , double ));
EXTERN bdd_t *bdd_approx_biased_rua ARGS((bdd_t *, bdd_approx_dir_t , bdd_t *, int , int , double, double ));
EXTERN bdd_t *bdd_approx_compress ARGS((bdd_t *, bdd_approx_dir_t , int , int ));
EXTERN int bdd_var_decomp ARGS((bdd_t *, bdd_partition_type_t , bdd_t ***));
EXTERN int bdd_gen_decomp ARGS((bdd_t *, bdd_partition_type_t , bdd_t ***));
EXTERN int bdd_approx_decomp ARGS((bdd_t *, bdd_partition_type_t , bdd_t ***));
EXTERN int bdd_iter_decomp ARGS((bdd_t *, bdd_partition_type_t , bdd_t ***));
EXTERN bdd_t *bdd_shortest_path ARGS((bdd_t *f, int *weight, int *support, int *length));

EXTERN bdd_t *bdd_compute_cube ARGS((bdd_manager *mgr, array_t *vars));
EXTERN bdd_node *bdd_add_compose ARGS((bdd_manager *mgr, bdd_node *fn1, bdd_node *fn2, int var));
EXTERN bdd_node *bdd_add_xnor ARGS((bdd_manager *mgr, bdd_node **fn1, bdd_node **fn2 ));
EXTERN bdd_node *bdd_add_times ARGS((bdd_manager *mgr, bdd_node **fn1, bdd_node **fn2));
EXTERN bdd_node *bdd_add_vector_compose ARGS((bdd_manager *mgr, bdd_node *fn, bdd_node **vector));
EXTERN bdd_node *bdd_add_residue ARGS((bdd_manager *mgr, int n, int m, int options, int top));
EXTERN bdd_node *bdd_add_nonsim_compose ARGS((bdd_manager *mgr, bdd_node *fn, bdd_node **vector));
EXTERN bdd_node *bdd_add_apply ARGS((bdd_manager *mgr, bdd_node *(*operation)(bdd_manager *, bdd_node **, bdd_node **), bdd_node *fn1, bdd_node *fn2));
EXTERN bdd_node *bdd_add_exist_abstract ARGS((bdd_manager *mgr, bdd_node *fn, bdd_node *vars));
EXTERN void bdd_recursive_deref ARGS((bdd_manager *mgr, bdd_node *f));
EXTERN void bdd_ref ARGS((bdd_node *fn));
EXTERN bdd_node *bdd_bdd_to_add ARGS((bdd_manager *mgr, bdd_node *fn));
EXTERN bdd_node *bdd_add_permute ARGS((bdd_manager *mgr, bdd_node *fn, int *permut));
EXTERN bdd_node *bdd_bdd_permute ARGS((bdd_manager *mgr, bdd_node *fn, int *permut));
EXTERN bdd_node * bdd_bdd_exist_abstract ARGS((bdd_manager *mgr, bdd_node *fn, bdd_node *cube));
/* Added by Balakrishna Kumthekar. There are potential duplicates. */

EXTERN int  bdd_equal_sup_norm  ARGS((bdd_manager *mgr, bdd_node *fn, bdd_node *gn, BDD_VALUE_TYPE tolerance, int pr));
EXTERN bdd_node * bdd_read_logic_zero ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_bdd_ith_var ARGS((bdd_manager *mgr, int i));
EXTERN bdd_node * bdd_add_divide ARGS((bdd_manager *mgr, bdd_node **fn1, bdd_node **fn2));
EXTERN bdd_node * bdd_bdd_constrain ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *c));
EXTERN bdd_node * bdd_bdd_restrict ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *c));
EXTERN bdd_node * bdd_add_hamming ARGS((bdd_manager *mgr, bdd_node **xVars, bdd_node **yVars, int nVars));
EXTERN bdd_node * bdd_add_ite ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g, bdd_node *h));
EXTERN bdd_node * bdd_add_find_max ARGS((bdd_manager *mgr, bdd_node *f));
EXTERN int bdd_bdd_pick_one_cube ARGS((bdd_manager *mgr, bdd_node *node, char *string));
EXTERN bdd_node * bdd_add_swap_variables ARGS((bdd_manager *mgr, bdd_node *f, bdd_node **x, bdd_node **y, int n));
EXTERN bdd_node * bdd_bdd_or ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_bdd_compute_cube ARGS((bdd_manager *mgr, bdd_node **vars, int *phase, int n));
EXTERN bdd_node * bdd_indices_to_cube ARGS((bdd_manager *mgr, int *idArray, int n));
EXTERN bdd_node * bdd_bdd_and ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_add_matrix_multiply ARGS((bdd_manager *mgr, bdd_node *A, bdd_node *B, bdd_node **z, int nz));
EXTERN bdd_node * bdd_add_compute_cube ARGS((bdd_manager *mgr, bdd_node **vars, int *phase, int n));
EXTERN bdd_node * bdd_add_const ARGS((bdd_manager *mgr, BDD_VALUE_TYPE c));
EXTERN bdd_node * bdd_bdd_swap_variables ARGS((bdd_manager *mgr, bdd_node *f, bdd_node **x, bdd_node **y, int n));
EXTERN double bdd_count_minterm ARGS((bdd_manager *mgr, bdd_node *f, int n));
EXTERN bdd_node * bdd_add_bdd_threshold ARGS((bdd_manager *mgr, bdd_node *f, BDD_VALUE_TYPE value));
EXTERN bdd_node * bdd_add_bdd_strict_threshold ARGS((bdd_manager *mgr,bdd_node *f,BDD_VALUE_TYPE value));
EXTERN BDD_VALUE_TYPE bdd_read_epsilon ARGS((bdd_manager *mgr)); 
EXTERN bdd_node * bdd_read_one ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_bdd_pick_one_minterm ARGS((bdd_manager *mgr, bdd_node *f, bdd_node **vars, int n));
EXTERN bdd_t * bdd_pick_one_minterm ARGS((bdd_t *f, array_t *varsArray));

/*Ashwini:start */
EXTERN  void bdd_bdd_srandom ARGS(( long seed));
EXTERN bdd_t * bdd_bdd_pick_one_minterm_random ARGS((bdd_t *f, array_t *varsArray,int n,int overAllVars));
EXTERN int bdd_bdd_eval_bdd ARGS((bdd_t *f, int *varsArray));
/*Ashwini: end*/

EXTERN array_t * bdd_bdd_pick_arbitrary_minterms ARGS((bdd_t *f, array_t *varsArray, int n, int k));
EXTERN bdd_t * bdd_subset_with_mask_vars ARGS((bdd_t *f, array_t *varsArray, array_t *maskVarsArray));
EXTERN bdd_node * bdd_read_zero ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_bdd_new_var ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_bdd_and_abstract ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g, bdd_node *cube));

EXTERN int bdd_test_unate ARGS((bdd_t *f, int varId, int phase));
EXTERN array_t * bdd_find_essential ARGS((bdd_t *));
EXTERN bdd_t * bdd_find_essential_cube ARGS((bdd_t *));
EXTERN void bdd_deref ARGS((bdd_node *f));
EXTERN bdd_node * bdd_add_plus ARGS((bdd_manager *mgr, bdd_node **fn1, bdd_node **fn2));
EXTERN int bdd_read_reorderings ARGS((bdd_manager *mgr));
EXTERN int bdd_read_next_reordering ARGS((bdd_manager *mgr));
EXTERN void bdd_set_next_reordering ARGS((bdd_manager *mgr, int next));
EXTERN bdd_node * bdd_bdd_xnor ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_bdd_xor ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_bdd_vector_compose ARGS((bdd_manager *mgr, bdd_node *f, bdd_node **vector));

EXTERN bdd_node * bdd_zdd_get_node ARGS((bdd_manager *mgr, int id, bdd_node *g, bdd_node *h));
EXTERN bdd_node * bdd_zdd_product ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_product_recur ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_union ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_union_recur ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_weak_div ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_weak_div_recur ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_isop_recur ARGS((bdd_manager *mgr, bdd_node *L, bdd_node *U, bdd_node **zdd_I));
EXTERN bdd_node * bdd_zdd_isop ARGS((bdd_manager *mgr, bdd_node *L, bdd_node *U, bdd_node **zdd_I));
EXTERN int bdd_zdd_get_cofactors3 ARGS((bdd_manager *mgr, bdd_node *f, int v, bdd_node **f1, bdd_node **f0, bdd_node **fd));
EXTERN bdd_node * bdd_bdd_and_recur ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_unique_inter ARGS((bdd_manager *mgr, int v, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_unique_inter_ivo ARGS((bdd_manager *mgr, int v, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_diff ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_zdd_diff_recur ARGS((bdd_manager *mgr, bdd_node *f, bdd_node *g));
EXTERN int bdd_num_zdd_vars ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_regular ARGS((bdd_node *f));
EXTERN int bdd_is_constant ARGS((bdd_node *f));
EXTERN int bdd_is_complement ARGS((bdd_node *f));
EXTERN bdd_node * bdd_bdd_T ARGS((bdd_node *f));
EXTERN bdd_node * bdd_bdd_E ARGS((bdd_node *f));
EXTERN bdd_node * bdd_not_bdd_node ARGS((bdd_node *f));
EXTERN void bdd_recursive_deref_zdd ARGS((bdd_manager *mgr, bdd_node *f));
EXTERN int bdd_zdd_count ARGS((bdd_manager *mgr, bdd_node *f));
EXTERN int bdd_read_zdd_level ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_zdd_vars_from_bdd_vars ARGS((bdd_manager *mgr, int multiplicity));
EXTERN void bdd_zdd_realign_enable ARGS((bdd_manager *mgr));
EXTERN void bdd_zdd_realign_disable ARGS((bdd_manager *mgr));
EXTERN int bdd_zdd_realignment_enabled ARGS((bdd_manager *mgr));
EXTERN void bdd_realign_enable ARGS((bdd_manager *mgr));
EXTERN void bdd_realign_disable ARGS((bdd_manager *mgr));
EXTERN int bdd_realignment_enabled ARGS((bdd_manager *mgr));
EXTERN int bdd_node_read_index ARGS((bdd_node *f));
EXTERN bdd_node * bdd_read_next ARGS((bdd_node *f));
/* This function should not be used by an external user. This will not be a
 * part of any future releases.
 */
EXTERN void bdd_set_next ARGS((bdd_node *f, bdd_node *g));
EXTERN bdd_node * bdd_add_apply_recur ARGS((bdd_manager *mgr, bdd_node *(*operation)(bdd_manager *, bdd_node **, bdd_node **), bdd_node *fn1, bdd_node *fn2));
EXTERN BDD_VALUE_TYPE bdd_add_value ARGS((bdd_node *f));

EXTERN bdd_node * bdd_read_plus_infinity ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_priority_select ARGS((bdd_manager *mgr,bdd_node *R,bdd_node **x,bdd_node **y,bdd_node **z,bdd_node *Pi,int n, bdd_node *(*Pifunc)(bdd_manager *, int, bdd_node **, bdd_node **, bdd_node **)));
EXTERN void bdd_set_background ARGS((bdd_manager *mgr,bdd_node *f));
EXTERN bdd_node * bdd_read_background ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_bdd_cofactor ARGS((bdd_manager *mgr,bdd_node *f,bdd_node *g));
EXTERN bdd_node * bdd_bdd_ite ARGS((bdd_manager *mgr,bdd_node *f,bdd_node *g,bdd_node *h));
EXTERN bdd_node * bdd_add_minus ARGS((bdd_manager *mgr,bdd_node **fn1,bdd_node **fn2));
EXTERN bdd_node * bdd_dxygtdxz ARGS((bdd_manager *mgr,int N,bdd_node **x, bdd_node **y, bdd_node **z));
EXTERN bdd_node * bdd_bdd_univ_abstract ARGS((bdd_manager *mgr,bdd_node *fn,bdd_node *vars));
EXTERN bdd_node * bdd_bdd_cprojection ARGS((bdd_manager *mgr,bdd_node *R,bdd_node *Y));
EXTERN double *bdd_cof_minterm ARGS((bdd_t *));
EXTERN int bdd_var_is_dependent ARGS((bdd_t *, bdd_t *));
EXTERN int bdd_debug_check ARGS((bdd_manager *mgr));
EXTERN bdd_node * bdd_xeqy ARGS((bdd_manager *mgr, int N, bdd_node **x, bdd_node **y));
EXTERN bdd_node * bdd_add_roundoff ARGS((bdd_manager *mgr, bdd_node *f, int N));
EXTERN bdd_node * bdd_xgty ARGS((bdd_manager *mgr, int N, bdd_node **x, bdd_node **y));
EXTERN bdd_node * bdd_add_cmpl ARGS((bdd_manager *mgr, bdd_node *f));
EXTERN bdd_node * bdd_split_set ARGS((bdd_manager *mgr, bdd_node *f, bdd_node ** x, int n, double m));

/*
 * Queries about BDD Formulas
 */
EXTERN boolean bdd_equal ARGS((bdd_t *, bdd_t *));
EXTERN boolean bdd_equal_mod_care_set ARGS((bdd_t *, bdd_t *, bdd_t *));
EXTERN boolean bdd_equal_mod_care_set_array ARGS((bdd_t *, bdd_t *, array_t *));
EXTERN bdd_t *bdd_intersects ARGS((bdd_t *, bdd_t *));
EXTERN bdd_t *bdd_closest_cube ARGS((bdd_t *, bdd_t *, int *));
EXTERN boolean bdd_is_tautology ARGS((bdd_t *, boolean));
EXTERN boolean bdd_leq ARGS((bdd_t *, bdd_t *, boolean, boolean));
EXTERN boolean bdd_lequal_mod_care_set ARGS((bdd_t *, bdd_t *, boolean, boolean, bdd_t *));
EXTERN boolean bdd_lequal_mod_care_set_array ARGS((bdd_t *, bdd_t *, boolean, boolean, array_t *));
EXTERN boolean bdd_leq_array ARGS((bdd_t *, array_t *, boolean, boolean));
EXTERN double bdd_count_onset ARGS((bdd_t *, array_t *));
EXTERN int bdd_epd_count_onset ARGS((bdd_t *, array_t *, EpDouble *epd));
EXTERN int bdd_print_apa_minterm ARGS((FILE *, bdd_t *, int, int));
EXTERN int bdd_apa_compare_ratios ARGS((int, bdd_t *, bdd_t *, int, int));

EXTERN double bdd_correlation ARGS((bdd_t *, bdd_t *));
EXTERN int bdd_get_free ARGS((bdd_t *));
EXTERN bdd_manager *bdd_get_manager ARGS((bdd_t *));
EXTERN bdd_node *bdd_get_node ARGS((bdd_t *, boolean *));
EXTERN bdd_node *bdd_extract_node_as_is ARGS((bdd_t *));
EXTERN var_set_t *bdd_get_support ARGS((bdd_t *));
EXTERN int bdd_is_support_var ARGS((bdd_t *, bdd_t *));
EXTERN int bdd_is_support_var_id ARGS((bdd_t *, int));
EXTERN array_t *bdd_get_varids ARGS((array_t *));
EXTERN unsigned int bdd_num_vars ARGS((bdd_manager *));
EXTERN int bdd_read_node_count ARGS((bdd_manager *mgr));
EXTERN void bdd_print ARGS((bdd_t *));
EXTERN int bdd_print_minterm ARGS((bdd_t *));
EXTERN void bdd_print_stats ARGS((bdd_manager *, FILE *));
EXTERN int bdd_set_parameters ARGS((bdd_manager *, avl_tree *valueTable, FILE *));
EXTERN int bdd_size ARGS((bdd_t *));
EXTERN int bdd_node_size ARGS((bdd_node *));
EXTERN long bdd_size_multiple ARGS((array_t *));
EXTERN boolean bdd_is_cube ARGS((bdd_t*));
EXTERN bdd_block * bdd_new_var_block(bdd_t *f, long length);
EXTERN long bdd_top_var_level ARGS((bdd_manager *manager, bdd_t *fn));
EXTERN bdd_variableId bdd_get_id_from_level ARGS((bdd_manager *, long));
EXTERN bdd_variableId bdd_top_var_id ARGS((bdd_t *));
EXTERN int bdd_get_level_from_id ARGS((bdd_manager *mgr, int id));
EXTERN int bdd_check_zero_ref ARGS((bdd_manager *mgr));
EXTERN int bdd_estimate_cofactor ARGS((bdd_t *, bdd_t *, int ));
/* Reordering related stuff */
EXTERN void bdd_dynamic_reordering ARGS((bdd_manager *, bdd_reorder_type_t, bdd_reorder_verbosity_t));
EXTERN void bdd_dynamic_reordering_zdd ARGS((bdd_manager *, bdd_reorder_type_t, bdd_reorder_verbosity_t));
EXTERN int bdd_reordering_status ARGS((bdd_manager *mgr, bdd_reorder_type_t *method));
EXTERN int bdd_reordering_zdd_status ARGS((bdd_manager *mgr, bdd_reorder_type_t *method));
EXTERN void bdd_reorder ARGS((bdd_manager *));
EXTERN int bdd_shuffle_heap ARGS((bdd_manager *mgr, int *permut));
EXTERN void bdd_dynamic_reordering_disable ARGS((bdd_manager *mgr));
EXTERN void bdd_dynamic_reordering_zdd_disable ARGS((bdd_manager *mgr));
EXTERN int bdd_read_reordered_field ARGS((bdd_manager *mgr));
EXTERN int bdd_add_hook ARGS((bdd_manager *, int (*procedure)(bdd_manager *, char *, void *), bdd_hook_type_t ));
EXTERN int bdd_remove_hook ARGS((bdd_manager *, int (*procedure)(bdd_manager *, char *, void *), bdd_hook_type_t ));
EXTERN int bdd_enable_reordering_reporting ARGS((bdd_manager *));
EXTERN int bdd_disable_reordering_reporting ARGS((bdd_manager *));
EXTERN bdd_reorder_verbosity_t bdd_reordering_reporting ARGS((bdd_manager *));
/* This function should not be used by an external user. It will
 * not be a part of any future release.
 */
EXTERN void bdd_set_reordered_field ARGS((bdd_manager *mgr, int n));
EXTERN bdd_node *bdd_bdd_vector_support ARGS((bdd_manager *mgr,bdd_node **F,int n));
EXTERN int bdd_bdd_vector_support_size ARGS((bdd_manager *mgr,bdd_node **F, int n));
EXTERN int bdd_bdd_support_size ARGS((bdd_manager *mgr,bdd_node *F));
EXTERN bdd_node *bdd_bdd_support ARGS((bdd_manager *mgr,bdd_node *F));
EXTERN bdd_node *bdd_add_general_vector_compose ARGS((bdd_manager *mgr,bdd_node *f,bdd_node **vectorOn,bdd_node **vectorOff));
EXTERN int bdd_bdd_leq ARGS((bdd_manager *mgr,bdd_node *f,bdd_node *g));
EXTERN bdd_node *bdd_bdd_boolean_diff ARGS((bdd_manager *mgr,bdd_node *f,int x));

/*
** Generator related functions.
** These are NOT to be used directly; only indirectly in the macros.
*/
EXTERN bdd_gen_status bdd_gen_read_status ARGS((bdd_gen *gen));
EXTERN bdd_gen *bdd_first_cube ARGS((bdd_t *, array_t **));
EXTERN boolean bdd_next_cube ARGS((bdd_gen *, array_t **));
EXTERN bdd_gen *bdd_first_node ARGS((bdd_t *, bdd_node **));
EXTERN boolean bdd_next_node ARGS((bdd_gen *, bdd_node **));
EXTERN int bdd_gen_free ARGS((bdd_gen *));

/* 
 * Miscellaneous
 */
EXTERN void bdd_set_gc_mode ARGS((bdd_manager *, boolean));
EXTERN bdd_external_hooks *bdd_get_external_hooks ARGS((bdd_manager *));
EXTERN bdd_t *bdd_construct_bdd_t ARGS((bdd_manager *mgr, bdd_node * fn));
EXTERN void bdd_dump_blif ARGS((bdd_manager *mgr, int nBdds, bdd_node **bdds, char **inames, char **onames, char *model, FILE *fp));
EXTERN void bdd_dump_blif_body ARGS((bdd_manager *mgr, int nBdds, bdd_node **bdds, char **inames, char **onames, FILE *fp));
EXTERN bdd_node *bdd_make_bdd_from_zdd_cover ARGS((bdd_manager *mgr, bdd_node *node));
EXTERN bdd_node *bdd_zdd_complement ARGS((bdd_manager *mgr, bdd_node *node));
EXTERN int bdd_ptrcmp ARGS((bdd_t *f, bdd_t *g));
EXTERN int bdd_ptrhash ARGS((bdd_t *f,int size));
EXTERN long bdd_read_peak_memory ARGS (( bdd_manager *mgr));
EXTERN int bdd_read_peak_live_node ARGS (( bdd_manager *mgr));
EXTERN int bdd_set_pi_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_set_ps_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_set_ns_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_pi_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_ps_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_ns_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_set_pair_index ARGS((bdd_manager *mgr, int index, int pairIndex));
EXTERN int bdd_read_pair_index ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_set_var_to_be_grouped ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_set_var_hard_group ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_reset_var_to_be_grouped ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_var_to_be_grouped ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_var_hard_group ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_var_to_be_ungrouped ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_set_var_to_be_ungrouped ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_bind_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_unbind_var ARGS((bdd_manager *mgr, int index));
EXTERN int bdd_is_lazy_sift ARGS((bdd_manager *mgr));
EXTERN void bdd_discard_all_var_groups ARGS((bdd_manager *mgr));
#endif 

