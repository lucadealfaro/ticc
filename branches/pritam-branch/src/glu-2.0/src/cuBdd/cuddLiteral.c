/**CFile***********************************************************************

  FileName    [cuddLiteral.c]

  PackageName [cudd]

  Synopsis    [Functions for manipulation of literal sets represented by
  BDDs.]

  Description [External procedures included in this file:
		<ul>
		<li> Cudd_bddLiteralSetIntersection()
		</ul>
	    Internal procedures included in this file:
		<ul>
		<li> cuddBddLiteralSetIntersectionRecur()
		</ul>]

  Author      [Fabio Somenzi]

  Copyright   [This file was created at the University of Colorado at
  Boulder.  The University of Colorado at Boulder makes no warranty
  about the suitability of this software for any purpose.  It is
  presented on an AS IS basis.]

******************************************************************************/

#include "util.h"
#include "cuddInt.h"


/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/

#ifndef lint
static char rcsid[] DD_UNUSED = "$Id: cuddLiteral.c,v 1.1 2005/04/21 05:58:00 luca Exp $";
#endif

/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/


/**AutomaticEnd***************************************************************/


/*---------------------------------------------------------------------------*/
/* Definition of exported functions                                          */
/*---------------------------------------------------------------------------*/


/**Function********************************************************************

  Synopsis    [Computes the intesection of two sets of literals
  represented as BDDs.]

  Description [Computes the intesection of two sets of literals
  represented as BDDs. Each set is represented as a cube of the
  literals in the set. The empty set is represented by the constant 1.
  No variable can be simultaneously present in both phases in a set.
  Returns a pointer to the BDD representing the intersected sets, if
  successful; NULL otherwise.]

  SideEffects [None]

******************************************************************************/
DdNode *
Cudd_bddLiteralSetIntersection(
  DdManager * dd,
  DdNode * f,
  DdNode * g)
{
    DdNode *res;

    do {
	dd->reordered = 0;
	res = cuddBddLiteralSetIntersectionRecur(dd,f,g);
    } while (dd->reordered == 1);
    return(res);

} /* end of Cudd_bddLiteralSetIntersection */


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/**Function********************************************************************

  Synopsis    [Performs the recursive step of
  Cudd_bddLiteralSetIntersection.]

  Description [Performs the recursive step of
  Cudd_bddLiteralSetIntersection. Scans the cubes for common variables,
  and checks whether they agree in phase.  Returns a pointer to the
  resulting cube if successful; NULL otherwise.]

  SideEffects [None]

******************************************************************************/
DdNode *
cuddBddLiteralSetIntersectionRecur(
  DdManager * dd,
  DdNode * f,
  DdNode * g)
{
    DdNode *res, *tmp;
    DdNode *F, *G;
    DdNode *fc, *gc;
    DdNode *one;
    DdNode *zero;
    unsigned int topf, topg, comple;
    int phasef, phaseg;

    statLine(dd);
    if (f == g) return(f);

    F = Cudd_Regular(f);
    G = Cudd_Regular(g);
    one = DD_ONE(dd);

    /* Here f != g. If F == G, then f and g are complementary.
    ** Since they are two cubes, this case only occurs when f == v,
    ** g == v', and v is a variable or its complement.
    */
    if (F == G) return(one);

    zero = Cudd_Not(one);
    topf = cuddI(dd,F->index);
    topg = cuddI(dd,G->index);
    /* Look for a variable common to both cubes. If there are none, this
    ** loop will stop when the constant node is reached in both cubes.
    */
    while (topf != topg) {
	if (topf < topg) {	/* move down on f */
	    comple = f != F;
	    f = cuddT(F);
	    if (comple) f = Cudd_Not(f);
	    if (f == zero) {
		f = cuddE(F);
		if (comple) f = Cudd_Not(f);
	    }
	    F = Cudd_Regular(f);
	    topf = cuddI(dd,F->index);
	} else if (topg < topf) {
	    comple = g != G;
	    g = cuddT(G);
	    if (comple) g = Cudd_Not(g);
	    if (g == zero) {
		g = cuddE(G);
		if (comple) g = Cudd_Not(g);
	    }
	    G = Cudd_Regular(g);
	    topg = cuddI(dd,G->index);
	}
    }

    /* At this point, f == one <=> g == 1. It suffices to test one of them. */
    if (f == one) return(one);

    res = cuddCacheLookup2(dd,Cudd_bddLiteralSetIntersection,f,g);
    if (res != NULL) {
	return(res);
    }

    /* Here f and g are both non constant and have the same top variable. */
    comple = f != F;
    fc = cuddT(F);
    phasef = 1;
    if (comple) fc = Cudd_Not(fc);
    if (fc == zero) {
	fc = cuddE(F);
	phasef = 0;
	if (comple) fc = Cudd_Not(fc);
    }
    comple = g != G;
    gc = cuddT(G);
    phaseg = 1;
    if (comple) gc = Cudd_Not(gc);
    if (gc == zero) {
	gc = cuddE(G);
	phaseg = 0;
	if (comple) gc = Cudd_Not(gc);
    }

    tmp = cuddBddLiteralSetIntersectionRecur(dd,fc,gc);
    if (tmp == NULL) {
	return(NULL);
    }

    if (phasef != phaseg) {
	res = tmp;
    } else {
	cuddRef(tmp);
	if (phasef == 0) {
	    res = cuddBddAndRecur(dd,Cudd_Not(dd->vars[F->index]),tmp);
	} else {
	    res = cuddBddAndRecur(dd,dd->vars[F->index],tmp);
	}
	if (res == NULL) {
	    Cudd_RecursiveDeref(dd,tmp);
	    return(NULL);
	}
	cuddDeref(tmp); /* Just cuddDeref, because it is included in result */
    }

    cuddCacheInsert2(dd,Cudd_bddLiteralSetIntersection,f,g,res);

    return(res);

} /* end of cuddBddLiteralSetIntersectionRecur */


/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/

