/*
 * $Id: mlglu.c,v 1.46 2006/04/24 23:29:38 pritam Exp $
 *
 */

// Glu headers
#include <mdd.h>
#include <error.h>
#include <array.h>

#include <stdio.h>

// Caml-C interface headers
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>


/** *************************************************************** **/
/** Macros for extracting information from Caml custom blocks       **/

// Unwrap a manager
/* Note: Data_custom_val(x) returns a pointer (of type "void *") to *
 * the data part of the custom block v and the pointer must be cast *
 * to the type of the data contained in the custom block            */
#define Manager(x)  ((*(mdd_manager **) (Data_custom_val(x))))

// Unwrap a node
#define Mdd_mgr(x)  ((*(mlglu_node **)  (Data_custom_val(x))) -> mgr)
#define Mdd_node(x) ((*(mlglu_node **)  (Data_custom_val(x))) -> node)
#define Mdd_ptr(x)  ((*(mlglu_node **)  (Data_custom_val(x))))

// Unwrap a pointer
/* Note: Field(v, n) returns the value contained in the n-th field *
 * of the structured block. Fields are numbered from 0 to          *
 * Wosize_val(v)-1, where Wosize_val(v) returns the size of the    *
 * block v in words                                                */ 
#define Mdd_bvar_ptr(x) ((bvar_type *) (Field((x),0)))
#define Mdd_mvar_ptr(x) ((mvar_type *) (Field((x),0)))

// Useful assertions
#define assert_mgr(m) {if (m == NULL) raise_mgrError(); }
#define assert_same_mgr(m1, m2) {if (m1 != m2) raise_diffMgr(); }
//#define assert_array(a) {if (array_n (a) == 0) raise_arrayNull(); }
#define assert_array(a) {if (a == NIL(array_t)) raise_arrayNull(); }

typedef struct mlglu_node mlglu_node; //mdd nodes 

struct mlglu_node
{
  mdd_manager *mgr;  // pointer to the MDD manager
  mdd_t       *node; // pointer to mdd node
};


/** *************************************************************** **/
/** Static prototypes                                               **/

value Array_int_val  (array_t *); //function to convert an integer array into value
value Array_mval_val (array_t *); //function to convert an mdd array into value
value Array_bval_val (array_t *); //function to convert a bdd array into value

/********************************************************************/
/** External Function Declarations                                 **/

void utilMddPrintRec(mdd_manager *, mdd_t *, int);
void utilMddPrintRec2(mdd_manager *, mdd_t *, int);
char* utilMddToString(mdd_manager *, mdd_t *, int);

/** *************************************************************** **/

/* Funtion to raise the InvalidMgr exception
 * raised when the mdd manger in qusetion has not been initialized
 */
void raise_mgrError()
{
  raise_constant (*caml_named_value ("invalid mgr"));
}

/* Function to raise the DiffMgr exception
 * raised when two MDD operands belong to diff. managers
 */ 
void raise_diffMgr ()
{
  raise_constant (*caml_named_value ("diff mgr"));
}

/* Function to raise the NullArray exception
 * raised when an array is null
 */
void raise_arrayNull ()
{
  raise_constant (*caml_named_value ("null array"));
}

/* Finalization function for a MDD node which free()'s the custom blocks.*
 * allocation would call stat_alloc and store the address in the custom  *
 * block, finalization would call stat_free() on the address             *   
   
*/
static void mlglu_nodeFinalize (value foo)
{
  CAMLparam1 (foo);
  if (Manager (foo) != NULL) {
    mdd_free (Mdd_node (foo)); //tries to free mdd_t (bdd_t) node from the manager 
  }
  stat_free ((mlglu_node *) Mdd_ptr (foo)); //frees the custom block malloc()'ed 
  CAMLreturn0;
}

/* Wrapper for mdd node. Allocates space for mlglu_node
 * and creates a finalized block. Unlike Cudd, reference count
 * is not incremented for mdds.
 */

value mlglu_nodeWrap (mdd_manager *mgr, mdd_t *node)
{
  CAMLparam0 ();
  CAMLlocal1(result);
  mlglu_node *n;
  if (mgr == NULL) {
    raise_mgrError ();
  } else {

    /* A custom block is statically allocated essentially doing a malloc 
    *  and a pointer to malloc()'ed blocks is returned   
    *  if out of memory it raises an exception */

    n = (mlglu_node *) stat_alloc (sizeof (mlglu_node));
    n -> mgr = mgr;
    n -> node = node;

    //The finalize function pointer is packed in the value
    //which will be called when finished the use of the node  

    result = alloc_final (2, &mlglu_nodeFinalize, 0, 1);
    Mdd_ptr (result) = n;
  }
  CAMLreturn (result);
}


/* Finalization function for MDD manager 
 */

static void mlglu_mgrFinalize (value foo)
{     
  CAMLparam1 (foo);
  if (Manager (foo) != NULL) {
    mdd_quit(Manager (foo));
  }
  CAMLreturn0; 
}

/* Wrapper of a MDD Manager.
 * The finalization function for the manager takes care of quitting
 */

value mlglu_mgrWrap(mdd_manager *mgr)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = alloc_final (2, &mlglu_mgrFinalize, 0, 1); 
  Manager (result) = mgr;
  CAMLreturn (result);
}

/* Wrapper of node of type "mvar_type *" */  
value mlglu_mvarWrap(mvar_type *mvar) 
{
  CAMLparam0 ();
  CAMLlocal1 (result);
  result = alloc (1, Abstract_tag); // a custom block of size 1 representing abstract data type
  Store_field (result, 0, (int) mvar); 
  CAMLreturn(result);
}

value mlglu_bvarWrap(bvar_type *bvar)
{
  CAMLparam0();
  CAMLlocal1 (result);
  result = alloc(1, Abstract_tag); // a custom block of size 1 representing abstract data type
  Store_field (result, 0, (int) bvar);
  CAMLreturn (result);
}


// Functions to wrap data structures
/* Not needed anymore
 * 
char * packageType[3] = {"CMU", "CAL", "CUDD"};
char * genStatus[2] = {"BDD_EMPTY", "BDD_NONEMPTY"};
char * hookType[4] = {"BDD_PRE_GC_HOOK", 
		       "BDD_POST_GC_HOOK",
		       "BDD_PRE_REORDERING_HOOK", 
		       "BDD_POST_REORDERING_HOOK"};
char * approxDir[2] = {"BDD_OVER_APPROX", "BDD_UNDER_APPROX"};
char * partitionType[2] = {"BDD_CONJUNCTS", "BDD_DISJUNCTS"};
char * reorderVerbosity[6] = { "BDD_APPROX_HB",
			       "BDD_APPROX_SP",
			       "BDD_APPROX_COMP",
			       "BDD_APPROX_UA",
			       "BDD_APPROX_RUA",
			       "BDD_APPROX_BIASED_RUA" };
char * reorderingType[23] = { "BDD_REORDER_SIFT", 
			      "BDD_REORDER_WINDOW",
			      "BDD_REORDER_SAME",
			      "BDD_REORDER_RANDOM",
			      "BDD_REORDER_RANDOM_PIVOT", 
			      "BDD_REORDER_SIFT_CONVERGE",
			      "BDD_REORDER_SYMM_SIFT",
			      "BDD_REORDER_SYMM_SIFT_CONV",
			      "BDD_REORDER_LINEAR",
			      "BDD_REORDER_LINEAR_CONVERGE",
			      "BDD_REORDER_EXACT",
			      "BDD_REORDER_WINDOW2",
			      "BDD_REORDER_WINDOW3",
			      "BDD_REORDER_WINDOW4",
			      "BDD_REORDER_WINDOW2_CONV",
			      "BDD_REORDER_WINDOW3_CONV",
			      "BDD_REORDER_WINDOW4_CONV",
			      "BDD_REORDER_GROUP_SIFT",
			      "BDD_REORDER_GROUP_SIFT_CONV",
			      "BDD_REORDER_ANNEALING",
			      "BDD_REORDER_GENETIC",
			      "BDD_REORDER_LAZY_SIFT",
			      "BDD_REORDER_NONE" };
*/

bdd_package_type_t Packagetype_val (value package_type)
{
	CAMLparam1 (package_type);
	CAMLreturn( Int_val (package_type) );
}

bdd_gen_status Genstatus_val (value gen_status)
{
	CAMLparam1 (gen_status);
	CAMLreturn( Int_val (gen_status) );
}

bdd_hook_type_t Hooktype_val (value hook_type)
{
	CAMLparam1 (hook_type);
	CAMLreturn( Int_val (hook_type) );
}

bdd_approx_dir_t Aproxdir_val (value approx_dir)
{
	CAMLparam1 (approx_dir);
	CAMLreturn( Int_val (approx_dir) );
}

bdd_partition_type_t Partitiontype_val (value partition_type)
{
	CAMLparam1 (partition_type);
	CAMLreturn( Int_val (partition_type) );
}

bdd_reorder_verbosity_t Reorderverbosity_val (value reorder_verbosity)
{
	CAMLparam1 (reorder_verbosity);
	CAMLreturn( Int_val (reorder_verbosity) );
}

bdd_reorder_type_t Reordertype_val (value reorder_type)
{
  CAMLparam1 (reorder_type);
  CAMLreturn ( Int_val(reorder_type) );
}


/** *************************************************************** **/
/** Functions to make array_t from Ocaml list.                      **/

/* This function expects a list of Ocaml mdd's. */ 
array_t *Mdd_array_val (value l)
{
  array_t *result;
  CAMLlocal1 (tmp);
  //The list h::t are represented as block with size 2 and tag 0
  //The first field contains h, the second field t

  tmp = l; // Obtain the whole list 

  if (! Is_block (tmp) ) return NIL(array_t); //when list is empty 
  result = array_alloc (mdd_t *, 0);

  //until the list is a block type, insert the value of h into an 
  //array and list becomes t    
  while ( Is_block(tmp) ) {
    array_insert_last (mdd_t *, result, Mdd_node(Field (tmp, 0)));
    tmp = Field (tmp, 1);
  }
  return result;
}

/* This function expects a list of Ocaml int's
   and returns an array_t of int and returns an empty array for empty list. */ 
// Note: Same as Mdd_array_val only made of integers
array_t *Int_array_val (value l)
{
  array_t *result;
  CAMLlocal1 (tmp);
  int n;
  tmp = l;
  if (! Is_block (tmp) ) return array_alloc (int, 0); //returns a 0 element list 
  
  result = array_alloc (int, 0);
  while ( Is_block(tmp) ) {
    n = Int_val (Field (tmp, 0)); // convert the value into integer 
    array_insert_last (int, result, n);
    tmp = Field (tmp,1);
	}
  return result;
}

/* This function expects a list of Ocaml int's
   and returns an array_t of int. */ 
array_t *Int_array_or_nil_val (value l)
{
  array_t *result;
  CAMLlocal1 (tmp);
  int n;
  tmp = l;
  if (! Is_block (tmp) ) return NIL(array_t); //returns nil pointer 
  
  result = array_alloc (int, 0);
  while ( Is_block(tmp) ) {
    n = Int_val (Field (tmp, 0));
    array_insert_last (int, result, n);
    tmp = Field (tmp,1);
	}
  return result;
}

/* This function expects a list of Ocaml strings
   and returns an array_t of (char *) */ 

// for string values
array_t *String_array_val (value l)
{
  array_t *result;
  CAMLlocal1 (tmp);
  tmp = l;

  if (! Is_block (tmp) ) return NIL(array_t);
  result = array_alloc (char *, 1);

  while ( Is_block (tmp) ) {
    array_insert_last (char *, result, String_val (Field (tmp,0)));
    tmp = Field (tmp,1);
  }
  return result;
}

/* This function expects a list of Ocaml strings
   and returns an array_t of bvar_type */
array_t *Bvar_array_val (value l)
{
  array_t *result;
  CAMLlocal1 (tmp);
  tmp = l;
  if (! Is_block (tmp) ) return NIL(array_t);
  result = array_alloc (bvar_type , 0);
  while ( Is_block(tmp) ) {
    array_insert_last (bvar_type , result, *((bvar_type *) Mdd_bvar_ptr (Field (tmp, 0))));
    tmp = Field (tmp,1);
  }
  return result;
}


/** *************************************************************** **/
/** Functions to translate an array_t to an Ocaml list.             **/

/* This function expects an (array_t *) of int
   and returns a list (block) of int */
// Note: The function obtains a C array of integers and constructs 
// a list  
value Val_array_int (array_t *array)
{
  CAMLparam0();
  CAMLlocal1 (result);
  CAMLlocal1 (old_result);
  int array_size = array_n (array); //number of elements in the array   
  int i;
  result = Val_int (0); // []; equivalent to empty list; such that Is_block(old_result) == false  
  for (i = 0; i < array_size; i++) // for each 
    {
      old_result = result;
      result = alloc (2, 0); // allocate custom block of size 2, tag 0 (for list) 
      Store_field (result, 0, Val_int (array_fetch (int, array, i))); //new head 
      Store_field (result, 1, old_result); // new tail = old list 
    }
  CAMLreturn(result);
}

/* This function expects an (array_t *) of mvar_type
   and returns a list (block) of mvar_type */

//Note: similar as above only with ADT mvar_type
//Hence mvar are needed to wrap by  mlglu_mvarWrap function    
value Val_array_mvar (array_t *array)
{
  CAMLparam0();
  CAMLlocal1 (result);
  CAMLlocal1 (old_result);
  int i;
  int array_size = array_n (array);
  result = Val_int (0);
  for (i = 0; i < array_size; i++)
    {
      result = old_result;
      result = alloc (2, 0);
      Store_field (result, 0, mlglu_mvarWrap (array_fetch_p (mvar_type , array, i)));
      Store_field (result, 1, old_result);
    }
  CAMLreturn(result);
}

/* This function expects an (array_t *) of bvar_type
   and returns a list (block) of bvar_type */
value Val_array_bvar (array_t *array)
{
  CAMLparam0();
  CAMLlocal1 (result);
  CAMLlocal1 (old_result);
  int array_size;
  int i;
  array_size = array_n (array);
  result = Val_int (0);
  for (i = 0; i < array_size; i++)
    {
      old_result = result;
      result = alloc (2, 0);
      Store_field (result, 0, mlglu_bvarWrap (array_fetch_p (bvar_type , array, i)));
      Store_field (result, 1, old_result);
    }
  CAMLreturn(result);
}


/* This function expects an (array_t *) of mdd_t
   and returns a list (block) of mdd_t */
value Val_array_mdd (array_t *array)
{
  CAMLparam0();
  CAMLlocal1 (result);
  CAMLlocal1 (old_result);
  mdd_t *elt;
  mdd_manager *mgr;
  int array_size;
  int i;
  array_size = array_n (array);
  result = Val_int (0);
  for (i = 0; i < array_size; i++)
    {
      old_result = result;
      elt = array_fetch (mdd_t *, array, i);
      mgr = mdd_get_manager (elt); // Obtain the mdd manager from the mdd node itself 
     //Note: could be done once for ooptimization since we do not allow
     //mdds of different managers in a single array  
      result = alloc (2, 0);
      Store_field (result, 0, mlglu_nodeWrap (mgr, elt));
      Store_field (result, 1, old_result);
    }
  CAMLreturn(result);
}

/* *************************************************************** */
/* Functions inherited from bdd package */

value mlglu_and (value f, value g, value f_phase, value g_phase)
{
	CAMLparam4 (f, g, f_phase, g_phase);
	mdd_t *result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr); // checks f_mgr is not null 
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr); //checks f_mgr and g_mgr are same
	result = (mdd_t *) bdd_and ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), Int_val(f_phase), Int_val(g_phase)); 

	CAMLreturn (mlglu_nodeWrap (f_mgr, result));
}

value mlglu_and_with_limit (value f, value g, value f_phase, value g_phase, value limit)
{
	CAMLparam5 (f, g, f_phase, g_phase, limit);
	mdd_t *result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (mdd_t *) bdd_and_with_limit ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), Int_val(f_phase), Int_val(g_phase), Int_val(limit));
	CAMLreturn (mlglu_nodeWrap (f_mgr, result));
}

value mlglu_and_array (value f, value g_array, value f_phase, value g_phase)
{
	CAMLparam4 (f, g_array, f_phase, g_phase);
	mdd_t *result;
	mdd_manager *mgr;
	array_t *array;
	mdd_t *elt;
	mdd_manager *g_mgr;
	int i, array_size;
	mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr(mgr);
	array = Mdd_array_val (g_array);
	array_size = array_n (array);
	for (i = 0; i < array_size; i++) 
	{
		elt = array_fetch (mdd_t *, array, i);
		g_mgr = mdd_get_manager (elt);
		assert_mgr (g_mgr);
		assert_same_mgr (mgr, g_mgr);
	}
	result = (mdd_t *) bdd_and_array ((bdd_t *) Mdd_node (f), array, Int_val(f_phase), Int_val(g_phase));
	CAMLreturn (mlglu_nodeWrap (mgr, result));
}

value mlglu_dup (value f)
{
	CAMLparam1 (f);
	mdd_t *result;
	mdd_manager *mgr;
	mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr(mgr);
	result = (mdd_t *) bdd_dup ((bdd_t *) Mdd_node (f));
	CAMLreturn (mlglu_nodeWrap (mgr, result));
}

value mlglu_equal (value f, value g)
{
	CAMLparam2 (f, g);
	int result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (int) bdd_equal ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g));
	CAMLreturn (Val_int (result));
}

value mlglu_equal_mod_care_set (value f, value g, value careSet)
{
	CAMLparam3 (f, g, careSet);
	int result;
  	mdd_manager *f_mgr;
  	mdd_manager *g_mgr;
  	mdd_manager *careSet_mgr;
  	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
  	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
  	careSet_mgr = mdd_get_manager ((mdd_t *) Mdd_node (careSet));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	assert_same_mgr(f_mgr, careSet_mgr);
  	result = (int) bdd_equal_mod_care_set ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), (bdd_t *) Mdd_node (careSet));
  	CAMLreturn (Val_int (result));
}

value mlglu_closest_cube (value f, value g)
{
	CAMLparam2 (f, g);
	CAMLlocal1 (tuple);
	mdd_t *result;
	int dist;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (mdd_t *) bdd_closest_cube ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), (& dist));
	tuple = alloc_tuple(2);
	Field (tuple, 0) = mlglu_nodeWrap (f_mgr, result);
	Field (tuple, 1) = Val_int (dist);
	CAMLreturn (tuple);
}

value mlglu_free (value f)
{
	CAMLparam1 (f);
  	mdd_manager *mgr;
  	mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr(mgr);
  	bdd_free ((bdd_t *) Mdd_node (f));
  	CAMLreturn (Val_unit);
}

value mlglu_get_manager (value f)
{
  CAMLparam1 (f);
  CAMLlocal1 (result);
  mdd_manager *mgr;

  mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
  assert_mgr(mgr);

  /* The manager we return should _not_ be closed when
     the corresponding Ocaml variable is garbage-collected.
     So, we set the finalization function to be "custom_finalize_default".
     We still need to call "alloc_final" to keep our usual format
     for wrapped managers.
  */
  result = alloc_final (2, custom_finalize_default, 0, 1);
  Manager (result) = mgr;
  CAMLreturn (result);
}

value mlglu_is_tautology (value f, value phase)
{
	CAMLparam2 (f, phase);
  	int result;
  	mdd_manager *mgr;
  	mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr(mgr);
  	result = (int) bdd_is_tautology ((bdd_t *) Mdd_node (f), Int_val (phase));
  	CAMLreturn (Val_int (result));
}

value mlglu_ite (value i, value t, value e, value i_phase, value t_phase, value e_phase)
{
	CAMLparam5 (i, t, e, i_phase, t_phase);
  	CAMLxparam1 (e_phase);
  	mdd_t *result;
  	mdd_manager *i_mgr;
  	mdd_manager *t_mgr;
  	mdd_manager *e_mgr;
  	i_mgr = mdd_get_manager ((mdd_t *) Mdd_node (i));
  	t_mgr = mdd_get_manager ((mdd_t *) Mdd_node (t));
  	e_mgr = mdd_get_manager ((mdd_t *) Mdd_node (e));
	assert_mgr(i_mgr);
	assert_mgr(t_mgr);
	assert_mgr(e_mgr);
	assert_same_mgr(i_mgr, t_mgr);
	assert_same_mgr(i_mgr, e_mgr);
  	result = (mdd_t *) bdd_ite ((bdd_t *) Mdd_node (i), (bdd_t *) Mdd_node (t), (bdd_t *) Mdd_node (e), Int_val (i_phase), Int_val (t_phase), Int_val (e_phase));
  	CAMLreturn (mlglu_nodeWrap (i_mgr, result));
}

value mlglu_lequal (value f, value g, value f_phase, value g_phase)
{
	CAMLparam4 (f, g, f_phase, g_phase);
	int result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (int) bdd_leq ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), Int_val(f_phase), Int_val(g_phase));
	CAMLreturn (Val_int (result));
}

value mlglu_lequal_mod_care_set (value f, value g, value f_phase, value g_phase, value careSet)
{
	CAMLparam5 (f, g, f_phase, g_phase, careSet);
	int result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	mdd_manager *careSet_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	careSet_mgr = mdd_get_manager ((mdd_t *) Mdd_node (careSet));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_mgr(careSet_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	assert_same_mgr(f_mgr, careSet_mgr);
	result = (int) bdd_lequal_mod_care_set ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), Int_val (f_phase), Int_val (g_phase), (bdd_t *) Mdd_node (careSet));
	CAMLreturn (Val_int (result));
}

value mlglu_lequal_array (value f, value g_array, value f_phase, value g_phase)
{
	CAMLparam4 (f, g_array, f_phase, g_phase);
	int i, array_size, result;
	array_t *array;
	mdd_t *elt;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr (f_mgr);
	array = Mdd_array_val (g_array);
	array_size = array_n (array);
	for (i = 0; i < array_size; i++) 
	{
		elt = array_fetch (mdd_t *, array, i);
		g_mgr = mdd_get_manager (elt);
		assert_mgr (g_mgr);
		assert_same_mgr (f_mgr, g_mgr);
	}
	result = (int) bdd_leq_array ((bdd_t *) Mdd_node (f), array, Int_val (f_phase), Int_val (g_phase));
	CAMLreturn (Val_int (result));
}

value mlglu_multiway_and (value mgr, value bddArray)
{
	CAMLparam2 (mgr, bddArray);
	mdd_t *result;
	int i, array_size;
	array_t *array;
	mdd_t *elt;
	mdd_manager *g_mgr;
	array = Mdd_array_val (bddArray);
	array_size = array_n (array);
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	for (i = 0; i < array_size; i++) 
	{
		elt = array_fetch (mdd_t *, array, i);
		g_mgr = mdd_get_manager (elt);
		assert_mgr (g_mgr);
		assert_same_mgr (Manager (mgr), g_mgr);
	}
	result = (mdd_t *) bdd_multiway_and (Manager(mgr), array);
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_multiway_or (value mgr, value bddArray)
{
	CAMLparam2 (mgr, bddArray);
	mdd_t *result;
	int i, array_size;
	array_t *array;
	mdd_t *elt;
	mdd_manager *g_mgr;
	array = Mdd_array_val (bddArray);
	array_size = array_n (array);
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	for (i = 0; i < array_size; i++) 
	{
		elt = array_fetch (mdd_t *, array, i);
		g_mgr = mdd_get_manager (elt);
		assert_mgr (g_mgr);
		assert_same_mgr (Manager (mgr), g_mgr);
	}
	result = (mdd_t *) bdd_multiway_or (Manager(mgr), array);
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_multiway_xor (value mgr, value bddArray)
{
	CAMLparam2 (mgr, bddArray);
	mdd_t *result;
	int i, array_size;
	array_t *array;
	mdd_t *elt;
	mdd_manager *g_mgr;
	array = Mdd_array_val (bddArray);
	array_size = array_n (array);
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	for (i = 0; i < array_size; i++) 
	{
		elt = array_fetch (mdd_t *, array, i);
		g_mgr = mdd_get_manager (elt);
		assert_mgr (g_mgr);
		assert_same_mgr (Manager (mgr), g_mgr);
	}
	result = (mdd_t *) bdd_multiway_xor (Manager(mgr), array);
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_not (value f)
{
	CAMLparam1 (f);
	mdd_t *result;
	mdd_manager *mgr;
	mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr(mgr);
	result = (mdd_t *) bdd_not ((bdd_t *) Mdd_node (f));
	CAMLreturn (mlglu_nodeWrap (mgr, result));
}

value mlglu_one (value mgr)
{
	CAMLparam1 (mgr);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	result = (mdd_t *) bdd_one (Manager (mgr));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_or (value f, value g, value f_phase, value g_phase)
{
	CAMLparam4 (f, g, f_phase, g_phase);
	mdd_t *result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (mdd_t *) bdd_or ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g), Int_val (f_phase), Int_val (g_phase));
	CAMLreturn (mlglu_nodeWrap (f_mgr, result));
}

value mlglu_size (value f)
{
	CAMLparam1 (f);
	int result;
	mdd_manager *mgr;
	mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	assert_mgr(mgr);
	result = (int) bdd_size ((bdd_t *) Mdd_node (f));
	CAMLreturn (Val_int (result));
}

value mlglu_size_multiple (value f)
{
	CAMLparam1 (f);
	long int result;
	int i, array_size;
	array_t *array;
	mdd_t *elt1, *eltn;
	mdd_manager *f_mgr = NULL, *g_mgr = NULL;
	array = Mdd_array_val (f);
	array_size = array_n (array);
	if (array_size >= 1)
	{
		elt1 = array_fetch (mdd_t *, array, 0);
		f_mgr = mdd_get_manager (elt1);
		assert_mgr (f_mgr);
	}
	if (array_size >= 2)
	{
		for (i = 1; i < array_size; i++) 
		{
			eltn = array_fetch (mdd_t *, array, i);
			g_mgr = mdd_get_manager (eltn);
			assert_mgr (g_mgr);
			assert_same_mgr (f_mgr, g_mgr);
		}
	}
	result = bdd_size_multiple (array);
	CAMLreturn (Val_long (result));
}

value mlglu_top_var_id (value f)
{
	CAMLparam1 (f);
	int result;
	result = (int) bdd_top_var_id ((bdd_t *) Mdd_node (f));
	CAMLreturn (Val_int (result));
}

value mlglu_xor (value f, value g)
{
	CAMLparam2 (f, g);
	mdd_t *result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (mdd_t *) bdd_xor ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g));
	CAMLreturn (mlglu_nodeWrap (f_mgr, result));
}

value mlglu_xnor (value f, value g)
{
	CAMLparam2 (f, g);
	mdd_t *result;
	mdd_manager *f_mgr;
	mdd_manager *g_mgr;
	f_mgr = mdd_get_manager ((mdd_t *) Mdd_node (f));
	g_mgr = mdd_get_manager ((mdd_t *) Mdd_node (g));
	assert_mgr(f_mgr);
	assert_mgr(g_mgr);
	assert_same_mgr(f_mgr, g_mgr);
	result = (mdd_t *) bdd_xnor ((bdd_t *) Mdd_node (f), (bdd_t *) Mdd_node (g));
	CAMLreturn (mlglu_nodeWrap (f_mgr, result));
}

value mlglu_zero (value mgr)
{
	CAMLparam1 (mgr);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	result = (mdd_t *) bdd_zero (Manager (mgr));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}
	

/* Initialize an MDD Manager.
 * The finalizing function for the manager takes care of 
 * shutting down the manager, by caling mdd_quit.
 */
value mlglu_init (value mvar_values, value mvar_names, value mvar_strides)
{
	CAMLparam3 (mvar_values, mvar_names, mvar_strides);
	mdd_manager *M;
	array_t *values = Int_array_val (mvar_values);

	/* Tricky: when the user gives an empty list, "values" will be
	 a NULL pointer. But mdd_init does not accept NULL pointers,
	 so we change it to an empty array. */
	//if (values == NIL(array_t) ) 
	//values = array_alloc (int, 0);

	M = mdd_init (values, String_array_val (mvar_names), Int_array_or_nil_val (mvar_strides));
	CAMLreturn (mlglu_mgrWrap(M));
}

value mlglu_init_empty ()
{
	CAMLparam0 ();
	CAMLlocal1 (result);
	mdd_manager *M;

	M = mdd_init_empty ();
	CAMLreturn (mlglu_mgrWrap(M));
}

/* Functions to create variables
 */

value mlglu_create_variables (value mgr, value mvar_values, value mvar_names, value mvar_strides)
{
	CAMLparam4 (mgr, mvar_values, mvar_names, mvar_strides);
	unsigned int num;
        array_t *values = Int_array_val (mvar_values);

	/* Tricky: when the user gives an empty list, "values" will be
	 a NULL pointer. But mdd_create_variable does not accept NULL pointers,
	 so we change it to an empty array. */
	if (values == NIL(array_t) ) 
	  values = array_alloc (int, 0); 
	
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	num = mdd_create_variables (Manager (mgr), values, String_array_val (mvar_names), Int_array_or_nil_val (mvar_strides));
	CAMLreturn (Val_int (num));
	
}

value mlglu_create_variables_after (value mgr, value after_mvar_id, value mvar_values, value mvar_names, value mvar_strides)
{
	CAMLparam5 (mgr,after_mvar_id,mvar_values, mvar_names, mvar_strides);
	unsigned int num;

        if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
     
	num = mdd_create_variables_after (Manager (mgr), Int_val (after_mvar_id), Int_array_val (mvar_values), String_array_val (mvar_names), Int_array_or_nil_val (mvar_strides));
	CAMLreturn (Val_int (num));
	
}

value mlglu_create_variables_interleaved (value mgr, value inter_var_id, value no_mvars, value mvar_names)
{
	CAMLparam4 (mgr, inter_var_id, no_mvars, mvar_names);
	unsigned int num;
	
        if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	num = mdd_create_variables_interleaved (Manager(mgr), Int_val(inter_var_id), Int_val(no_mvars), String_array_val(mvar_names));
	CAMLreturn (Val_int (num));
	
}

// Functions to manipulate mdds
value mlglu_literal (value mgr, value mvar, value values)
{
	CAMLparam3 (mgr, mvar, values);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError ();
	}
	result = mdd_literal (Manager (mgr), Int_val (mvar), Int_array_val (values));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_case (value mgr, value mvar, value child_list)
{
	CAMLparam3 (mgr, mvar, child_list);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_case (Manager (mgr), Int_val (mvar), Int_array_val (child_list));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_range_mdd (value mgr, value support)
{
	CAMLparam2 (mgr, support);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_range_mdd (Manager (mgr), Int_array_val (support));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

/* Quits a MDD manager
 * This function calls mdd_quit and sets mgr to NULL
 * It is not a necessary to call this function -- the
 * finilization function for the manager also
 * calls mdd_quit, if not already done
 */

value mlglu_quit (value mgr)
{
	CAMLparam1 (mgr);
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	mdd_quit (Manager (mgr));
	Manager(mgr) = NULL;
	CAMLreturn (Val_unit);
}

value mlglu_array_free (value mddArray)
{
	CAMLparam1 (mddArray);
	mdd_array_free (Mdd_array_val (mddArray));
	CAMLreturn (Val_unit);
}


value mlglu_array_duplicate (value mddArray)
{
	CAMLparam1 (mddArray);
	array_t *result;
	array_t *array;
	array = Mdd_array_val (mddArray);
	assert_array (array);
	result = mdd_array_duplicate (array);
	CAMLreturn (Val_array_mdd (result));
}

value mlglu_array_equal (value mddArray1, value mddArray2)
{
	CAMLparam2 (mddArray1, mddArray2);
	array_t *array1, *array2;
	int result;
	array1 = Mdd_array_val (mddArray1);
	array2 = Mdd_array_val (mddArray2);
	assert_array (array1);
	assert_array (array2);
	result = mdd_array_equal (array1, array2);
	CAMLreturn (Val_int (result));
}

// Generic Functions
// Trick applied here is not thread-safe 
static value * mlglu_func1c_callback = NULL; 
boolean mlglu_func1c_func (int a, int b)
{
  return Bool_val (callback2 (*mlglu_func1c_callback, Val_int (a), Val_int (b)));
}
value mlglu_func1c (value mgr, value mvar1, value constant, value func2)
{
  CAMLparam4 (mgr, mvar1, constant, func2);
  mdd_t *result;
  if (Manager (mgr) == NULL) {
    raise_mgrError ();
  }
  mlglu_func1c_callback = &func2; 
  result = mdd_func1c (Manager (mgr), Int_val (mvar1), Int_val (constant), mlglu_func1c_func);
  CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

static value * mlglu_func2_callback = NULL;
boolean mlglu_func2_func (int a, int b)
{
  return Bool_val (callback2 (*mlglu_func2_callback, Val_int (a), Val_int (b)));
}
value mlglu_func2 (value mgr, value mvar1, value mvar2, value func2)
{
  CAMLparam4 (mgr, mvar1, mvar2, func2);
  mdd_t *result;
  if (Manager (mgr) == NULL) {
    raise_mgrError ();
  }
  mlglu_func2_callback = &func2; 
  result = mdd_func2 (Manager (mgr), Int_val (mvar1), Int_val (mvar2), mlglu_func2_func);
  CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

static value * mlglu_func2c_callback = NULL;
boolean mlglu_func2c_func (int a, int b, int c)
{
  return Bool_val (callback3 (*mlglu_func2c_callback, Val_int (a), Val_int (b), Val_int (c)));
}
value mlglu_func2c (value mgr, value mvar1, value mvar2, value constant, value func3)
{
  CAMLparam5 (mgr, mvar1, mvar2, constant, func3);
  mdd_t *result;
  if (Manager (mgr) == NULL) {
    raise_mgrError ();
  }
  mlglu_func2c_callback = &func3; 
  result = mdd_func2c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant), mlglu_func2c_func);
  CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

static value * mlglu_func3_callback = NULL;
boolean mlglu_func3_func (int a, int b, int c)
{
  return Bool_val (callback3 (*mlglu_func3_callback, Val_int (a), Val_int (b), Val_int (c)));
}
value mlglu_func3 (value mgr, value mvar1, value mvar2, value mvar3, value func3)
{
  CAMLparam5 (mgr, mvar1, mvar2, mvar3, func3);
  mdd_t *result;
  if (Manager (mgr) == NULL) {
    raise_mgrError ();
  }
  mlglu_func3_callback = &func3; 
  result = mdd_func3 (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3), mlglu_func3_func);
  CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

// Predicate relations between 2 variables and a constant
//For Variables with the same range
value mlglu_eq_s (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		
	}
	result = mdd_eq_s (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq_s (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq_s (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq_s (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq_s (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt_s (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt_s (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq_s (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq_s (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt_s (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt_s (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

//For variables with diffrerent ranges
//Predicate relations between a variable and a constant

value mlglu_eq_c (value mgr, value mvar1, value constant)
{
	CAMLparam3(mgr, mvar1, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_eq_c (Manager (mgr), Int_val (mvar1), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq_c (value mgr, value mvar1, value constant)
{
	CAMLparam3(mgr, mvar1, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq_c (Manager (mgr), Int_val (mvar1), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq_c (value mgr, value mvar1, value constant)
{
	CAMLparam3(mgr, mvar1, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq_c (Manager (mgr), Int_val (mvar1), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt_c (value mgr, value mvar1, value constant)
{
	CAMLparam3(mgr, mvar1, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt_c (Manager (mgr), Int_val (mvar1), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq_c (value mgr, value mvar1, value constant)
{
	CAMLparam3(mgr, mvar1, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq_c (Manager (mgr), Int_val (mvar1), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt_c (value mgr, value mvar1, value constant)
{
	CAMLparam3(mgr, mvar1, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt_c (Manager (mgr), Int_val (mvar1), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

//Predicate Relation between 2 variables
value mlglu_eq (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_eq (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt (value mgr, value mvar1, value mvar2)
{
	CAMLparam3(mgr, mvar1, mvar2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt (Manager (mgr), Int_val (mvar1), Int_val (mvar2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

// Predicate Relation between 2 variables and a constant
value mlglu_eq_plus_c (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_eq_plus_c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq_plus_c (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq_plus_c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq_plus_c (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq_plus_c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt_plus_c (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt_plus_c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq_plus_c (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq_plus_c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt_plus_c (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt_plus_c (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}


value mlglu_eq_minus_c_mod (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_eq_minus_c_mod (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq_minus_c_mod (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq_minus_c_mod (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq_minus_c_mod (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq_minus_c_mod (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt_minus_c_mod (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt_minus_c_mod (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq_minus_c_mod (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq_minus_c_mod (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt_minus_c_mod (value mgr, value mvar1, value mvar2, value constant)
{
	CAMLparam4 (mgr, mvar1, mvar2, constant);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt_minus_c_mod (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (constant));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

// Predicate relations between 3 variables
value mlglu_eq_plus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_eq_plus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq_plus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq_plus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq_plus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq_plus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt_plus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt_plus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq_plus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq_plus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt_plus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt_plus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_eq_minus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_eq_minus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_neq_minus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_neq_minus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_leq_minus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_leq_minus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_lt_minus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_lt_minus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_geq_minus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_geq_minus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_gt_minus (value mgr, value mvar1, value mvar2, value mvar3)
{
	CAMLparam4 (mgr, mvar1, mvar2, mvar3);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_gt_minus (Manager (mgr), Int_val (mvar1), Int_val (mvar2), Int_val (mvar3));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

// New useful operations
value mlglu_cofactor (value mgr, value fn, value cube)
{
	CAMLparam3 (mgr, fn, cube);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_cofactor (Manager (mgr), Mdd_node (fn), (mdd_t *) Mdd_node (cube));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}


value mlglu_cofactor_minterm (value fn, value minterm)
{
	CAMLparam2 (fn, minterm);
	mdd_t *result;
	mdd_manager *mgr;
	mgr = mdd_get_manager (Mdd_node (fn));
	assert_mgr(mgr);
	result = mdd_cofactor_minterm (Mdd_node (fn), (mdd_t *) Mdd_node (minterm));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_smooth (value mgr, value fn, value mvars)
{
	CAMLparam3 (mgr, fn, mvars);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	if ( Int_array_val(mvars) == NIL(array_t) || 
	      array_n(Int_array_val(mvars)) == 0) {
	    result = bdd_dup(Mdd_node(fn));
	} else {
	    result = mdd_smooth (Manager (mgr), Mdd_node (fn), Int_array_val (mvars));
	}
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_and_smooth (value mgr, value f, value g, value mvars)
{
	CAMLparam4 (mgr, f, g, mvars);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	if ( Int_array_val(mvars) == NIL( array_t )) {
	    result = ( bdd_and(Mdd_node(f), Mdd_node(g), 1, 1) ) ;
	} else if ( array_n(Int_array_val(mvars)) == 0)  {
	    result = ( bdd_and(Mdd_node(f), Mdd_node(g), 1, 1) ) ;
	} else {
	    result = mdd_and_smooth (Manager (mgr), (mdd_t *) Mdd_node (f), (mdd_t *) Mdd_node (g), Int_array_val (mvars));
	}
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_consensus (value mgr, value fn, value mvars)
{
	CAMLparam3 (mgr, fn, mvars);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	if ( Int_array_val(mvars) == NIL(array_t) || 
	     array_n(Int_array_val(mvars)) == 0) {
	    result = bdd_dup(Mdd_node(fn));
	} else {
	    result = mdd_consensus (Manager (mgr), Mdd_node (fn), Int_array_val (mvars));
	}
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));	
}

value mlglu_substitute (value mgr, value fn, value old_mvars, value new_mvars)
{
	CAMLparam4 (mgr, fn, old_mvars, new_mvars);
	mdd_t *result;
	array_t *old = Int_array_val (old_mvars);
	array_t *new = Int_array_val (new_mvars);

	/* Tricky: when the user gives an empty list, "values" will be
	 a NULL pointer. But mdd_substitute does not accept NULL pointers,
	 so we change it to an empty array. */
	if (old == NIL(array_t) ) {
	  old = array_alloc (int, 0);
	  new = array_alloc (int, 0);
	}

	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_substitute (Manager (mgr), Mdd_node (fn), old, new);
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}


value mlglu_get_support (value mgr, value fn)
{
	CAMLparam2 (mgr, fn);
	array_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_get_support (Manager (mgr), Mdd_node (fn));
	CAMLreturn (Val_array_int (result));
}

value mlglu_cproject (value mgr, value fn, value mvars)
{
	CAMLparam3 (mgr, fn, mvars);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_cproject (Manager (mgr), Mdd_node (fn), Int_array_val (mvars));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

// New functions added by Serdar
value mlglu_add_s (value mgr, value sum_id, value mvar_id1, value mvar_id2)
{
	CAMLparam4 (mgr, sum_id, mvar_id1, mvar_id2);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_add_s (Manager (mgr), Int_val (sum_id), Int_val (mvar_id1), Int_val (mvar_id2));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_mod (value mgr, value a_mvar_id, value b_mvar_id, value M)
{
	CAMLparam4 (mgr, a_mvar_id, b_mvar_id, M);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_mod (Manager (mgr), Int_val (a_mvar_id), Int_val (b_mvar_id), Int_val (M));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_build_leq_c (value mgr, value mvar_id, value c)
{
	CAMLparam3 (mgr, mvar_id, c);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = build_leq_c (Manager (mgr), Int_val (mvar_id), Int_val (c));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_build_lt_c (value mgr, value mvar_id, value c)
{
	CAMLparam3 (mgr, mvar_id, c);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = build_lt_c (Manager (mgr), Int_val (mvar_id), Int_val (c));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_build_geq_c (value mgr, value mvar_id, value c)
{
	CAMLparam3 (mgr, mvar_id, c);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = build_geq_c (Manager (mgr), Int_val (mvar_id), Int_val (c));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_build_gt_c (value mgr, value mvar_id, value c)
{
	CAMLparam3 (mgr, mvar_id, c);
	mdd_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = build_gt_c (Manager (mgr), Int_val (mvar_id), Int_val (c));
	CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

//This is a change because mdd_bundle_variables returns int but has a pointer
// as argument therefore we need to return (int*int)
value mlglu_bundle_variables (value mgr, value bundle_vars, value mdd_var_name)
{
	CAMLparam3 (mgr, bundle_vars, mdd_var_name);
	CAMLlocal1 (tuple);
	int mdd_id;
	int result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_bundle_variables (Manager (mgr), Int_array_val (bundle_vars), String_val (mdd_var_name), (& mdd_id));
	tuple = alloc_tuple(2);
	Field (tuple, 0) = Val_int (result);
	Field (tuple, 1) = Val_int (mdd_id);
	CAMLreturn (tuple);
}

value mlglu_ret_bvar (value mvar_ptr, value i, value bvar_list)
{
	CAMLparam3 (mvar_ptr, i, bvar_list);
	CAMLlocal1 (tuple);
	bvar_type result;
	result = mdd_ret_bvar ((mvar_type *) Mdd_mvar_ptr (mvar_ptr), Int_val(i), Bvar_array_val (bvar_list));
	CAMLreturn (mlglu_bvarWrap (&result));
}

value mlglu_ret_bvar_id (value mvar_ptr, value i)
{
	CAMLparam2 (mvar_ptr, i);
	int result;
	result = mdd_ret_bvar_id ((mvar_type *) Mdd_mvar_ptr (mvar_ptr), Int_val (i));
	CAMLreturn (Val_int (result));
}

value mlglu_ret_mvar_list (value mgr)
{
	CAMLparam1 (mgr);
	array_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_ret_mvar_list (Manager(mgr));
	CAMLreturn (Val_array_mvar (result));
}

value mlglu_ret_bvar_list (value mgr)
{
	
	CAMLparam1 (mgr);
	array_t *result;
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	result = mdd_ret_bvar_list (Manager (mgr));
	CAMLreturn (Val_array_bvar (result));
}

value mlglu_get_var_bits (value mgr, value id) 
{
	CAMLparam2 (mgr, id);
	mvar_type var;
	int result;
	if (Manager (mgr) == NULL) {
	  raise_mgrError();
	}
	var = mdd_get_var_by_id (Manager (mgr), Int_val (id));
	result = var.encode_length;
	CAMLreturn (Val_int (result));
}

value mlglu_get_var_range (value mgr, value id) 
{
	CAMLparam2 (mgr, id);
	mvar_type var;
	int result;
	if (Manager (mgr) == NULL) {
	  raise_mgrError();
	}
	var = mdd_get_var_by_id (Manager (mgr), Int_val (id));
	result = var.values;
	CAMLreturn (Val_int (result));
}

value mlglu_get_var_name (value mgr, value id) 
{
	CAMLparam2 (mgr, id);
	mvar_type var;

	if (Manager (mgr) == NULL) {
	  raise_mgrError();
	}
	var = mdd_get_var_by_id (Manager (mgr), Int_val (id));

	CAMLreturn (caml_copy_string(var.name));
}

// Reordering function
value mlglu_dynamic_reordering (value mgr, value reorder_type, value reorder_verbosity)
{
	CAMLparam3 (mgr, reorder_type, reorder_verbosity);
	if (Manager (mgr) == NULL) {
		raise_mgrError();
	}
	bdd_dynamic_reordering (Manager (mgr), Reordertype_val (reorder_type), Reorderverbosity_val (reorder_verbosity));
	CAMLreturn (Val_unit);
}
//MDD Printing Function 
value mlglu_print_mdd ( value mgr , value mdd )
{
 CAMLparam2 ( mgr , mdd );
 if (Manager (mgr) == NULL) {
    raise_mgrError();
 }
 utilMddPrintRec( Manager (mgr), Mdd_node (mdd), 0);
 CAMLreturn (Val_unit);
}

//MDD Mdd to string
value mlglu_mdd_to_string ( value mgr , value mdd )
{
  char* result;
  CAMLparam2 ( mgr , mdd );
  if (Manager (mgr) == NULL) {
    raise_mgrError();
  }
  result = utilMddToString( Manager (mgr), Mdd_node (mdd), 0);
  CAMLreturn (caml_copy_string(result));
}

value mlglu_pick_one_minterm (value mgr, value fn, value mvars)
{
  CAMLparam3 (mgr, fn, mvars);
  mdd_t *result;
  if (Manager (mgr) == NULL) {
    raise_mgrError();
  }
  if ( Int_array_val(mvars) == NIL(array_t) ) {
    result = bdd_dup(Mdd_node(fn));
  } else {
    result = mdd_pick_one_minterm (Manager (mgr), Mdd_node (fn), Int_array_val (mvars),
				   0 /* don't care. Unused */,
				   1 /* overAllVars since we want a fully assigned minterm */);
  }
  CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

value mlglu_pick_one_cube (value mgr, value fn, value mvars)
{
  CAMLparam3 (mgr, fn, mvars);
  mdd_t *result;
  if (Manager (mgr) == NULL) {
    raise_mgrError();
  }
  if ( Int_array_val(mvars) == NIL(array_t) ) {
    result = bdd_dup(Mdd_node(fn));
  } else {
    result = mdd_pick_one_minterm (Manager (mgr), Mdd_node (fn), Int_array_val (mvars),
				   0 /* don't care. Unused */,
				   0 /* not overAllVars as we want a cube */);
  }
  CAMLreturn (mlglu_nodeWrap (Manager (mgr), result));
}

/*
 * $Log: mlglu.c,v $
 * Revision 1.46  2006/04/24 23:29:38  pritam
 *
 *      array of variables (to be quantified out).
 *
 * Revision 1.45  2006/01/28 05:35:05  vishwa
 * Added a new function to mlglu. mdd_pick_one_cube which picks a random cube
 * given a stateset.
 *
 * Revision 1.44  2006/01/17 02:34:55  pritam
 * *** empty log message ***
 *
 * Revision 1.43  2006/01/16 16:59:35  mfaella
 * small fix to mlglu.c
 *
 * Revision 1.42  2005/12/29 16:10:16  mfaella
 * improved expression handling. Tests were reset.
 *
 * Revision 1.41  2005/12/26 17:32:15  mfaella
 * vars that only appear in invariants are listed among the module's vars
 *
 * Revision 1.40  2005/12/12 03:38:32  vishwa
 * Changed the simulate function to randomize on actions instead of on the
 * union of all possible reachable next states. Also, now generating html.
 * simulate takes a final filename parameter which is used to generate an
 * html file with the simulation results. Modified the printer tutorial
 * example to use the new simulate.
 *
 * Revision 1.39  2005/12/09 07:34:17  vishwa
 * Added mdd_pick_one_minterm to the glu package. Added function simulate
 * that does random simulation. Fixed a pathname in traffic.in.
 *
 * Revision 1.38  2005/08/20 02:54:16  mfaella
 * one bug fixed in mlglu, one patched in symbuild
 *
 * Revision 1.37  2005/08/08 23:25:03  thumper
 * Updated tests to work with the new output format.
 *
 * Revision 1.36  2005/08/04 23:49:02  thumper
 * Changes for compose.
 *
 * Revision 1.35.6.1  2005/08/04 23:48:09  thumper
 * Added configure script.
 *
 * Revision 1.35  2005/06/30 00:31:27  leandro
 * *** empty log message ***
 *
 * Revision 1.34  2005/06/29 01:47:46  mfaella
 * post added by Marco and Axel
 *
 * Revision 1.33  2005/05/20 23:18:14  leandro
 * *** empty log message ***
 *
 * Revision 1.32  2005/05/20 22:48:02  leandro
 * *** empty log message ***
 *
 * Revision 1.31  2005/05/20 18:28:15  pritam
 * *** empty log message ***
 *
 * Revision 1.30  2005/05/20 18:26:40  pritam
 * *** empty log message ***
 *
 * Revision 1.29  2005/05/05 20:22:32  leandro
 * *** empty log message ***
 *
 * Revision 1.28  2005/05/05 01:19:14  leandro
 * Some comments added, Val_array_mdd added, and test-scripts/test-mlglu-2 added
 *
 * Revision 1.27  2005/05/04 04:25:43  leandro
 * *** empty log message ***
 *
 * Revision 1.26  2005/05/04 03:18:48  pritam
 * *** empty log message ***
 *
 * Revision 1.25  2005/05/03 22:27:57  mfaella
 * *** empty log message ***
 *
 * Revision 1.24  2005/05/03 19:52:11  leandro
 * *** empty log message ***
 *
 * Revision 1.23  2005/05/03 19:38:15  mfaella
 * less bugs
 *
 * Revision 1.22  2005/05/03 02:48:36  leandro
 * *** empty log message ***
 *
 * Revision 1.21  2005/05/02 21:53:20  pritam
 * *** empty log message ***
 *
 * Revision 1.20  2005/05/02 21:51:27  pritam
 * *** empty log message ***
 *
 * Revision 1.18  2005/05/02 03:20:18  mfaella
 * *** empty log message ***
 *
 * Revision 1.17  2005/04/30 01:17:05  mfaella
 * bug fixed
 *
 * Revision 1.16  2005/04/29 23:48:24  leandro
 * *** empty log message ***
 *
 * Revision 1.15  2005/04/29 19:15:43  leandro
 * mdd_get_var_bits added
 *
 * Revision 1.14  2005/04/29 00:47:07  mfaella
 * some pointers fixed
 *
 * Revision 1.13  2005/04/28 23:31:22  leandro
 * *** empty log message ***
 *
 * Revision 1.12  2005/04/28 21:24:29  leandro
 * some types fixed
 *
 * Revision 1.11  2005/04/27 00:27:07  leandro
 * more functions wrapped
 *
 * Revision 1.10  2005/04/26 20:00:54  pritam
 * *** empty log message ***
 *
 * Revision 1.9  2005/04/26 18:18:01  leandro
 * more functions wrapped
 *
 * Revision 1.8  2005/04/26 03:03:25  pritam
 * *** empty log message ***
 *
 * Revision 1.7  2005/04/26 02:34:52  leandro
 * some bdd functions added
 *
 * Revision 1.6  2005/04/25 23:56:49  leandro
 * here are some updates
 *
 * Revision 1.5  2005/04/24 20:14:34  pritam
 * *** empty log message ***
 *
 * Revision 1.4  2005/04/23 18:54:57  mfaella
 * small fixes
 *
 * Revision 1.3  2005/04/23 02:40:03  leandro
 * *** empty log message ***
 *
 * Revision 1.2  2005/04/23 02:28:32  pritam
 * *** empty log message ***
 *
 * Revision 1.1  2005/04/22 22:56:43  leandro
 * *** empty log message ***
 *
 * Revision 1.1  2005/04/22 22:01:20  leandro
 * *** empty log message ***
 *
 * Revision 1.5  2005/04/22 21:59:27  leandro
 * *** empty log message ***
 *
 * Revision 1.4  2005/04/22 21:53:50  leandro
 * *** empty log message ***
 *
 *
 */
