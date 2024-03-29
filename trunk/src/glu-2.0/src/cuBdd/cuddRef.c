/**CFile***********************************************************************

  FileName    [cuddRef.c]

  PackageName [cudd]

  Synopsis    [Functions that manipulate the reference counts.]

  Description [External procedures included in this module:
		    <ul>
		    <li> Cudd_Ref()
		    <li> Cudd_RecursiveDeref()
		    <li> Cudd_IterDerefBdd()
		    <li> Cudd_DelayedDerefBdd()
		    <li> Cudd_RecursiveDerefZdd()
		    <li> Cudd_Deref()
		    <li> Cudd_CheckZeroRef()
		    </ul>
	       Internal procedures included in this module:
		    <ul>
		    <li> cuddReclaim()
		    <li> cuddReclaimZdd()
		    <li> cuddClearDeathRow()
		    <li> cuddShrinkDeathRow()
		    <li> cuddIsInDeathRow()
		    <li> cuddTimesInDeathRow()
		    </ul>
	      ]

  SeeAlso     []

  Author      [Fabio Somenzi]

  Copyright [ This file was created at the University of Colorado at
  Boulder.  The University of Colorado at Boulder makes no warranty
  about the suitability of this software for any purpose.  It is
  presented on an AS IS basis.]

******************************************************************************/

#include    "util.h"
#include    "cuddInt.h"

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
static char rcsid[] DD_UNUSED = "$Id: cuddRef.c,v 1.1 2005/04/21 05:58:00 luca Exp $";
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

  Synopsis [Increases the reference count of a node, if it is not
  saturated.]

  Description []

  SideEffects [None]

  SeeAlso     [Cudd_RecursiveDeref Cudd_Deref]

******************************************************************************/
void
Cudd_Ref(
  DdNode * n)
{

    n = Cudd_Regular(n);

    cuddSatInc(n->ref);

} /* end of Cudd_Ref */


/**Function********************************************************************

  Synopsis    [Decreases the reference count of node n.]

  Description [Decreases the reference count of node n. If n dies,
  recursively decreases the reference counts of its children.  It is
  used to dispose of a DD that is no longer needed.]

  SideEffects [None]

  SeeAlso     [Cudd_Deref Cudd_Ref Cudd_RecursiveDerefZdd]

******************************************************************************/
void
Cudd_RecursiveDeref(
  DdManager * table,
  DdNode * n)
{
    DdNode *N;
    int ord;
    DdNodePtr *stack = table->stack;
    int SP = 1;

    unsigned int live = table->keys - table->dead;
    if (live > table->peakLiveNodes) {
	table->peakLiveNodes = live;
    }

    N = Cudd_Regular(n);

    do {
#ifdef DD_DEBUG
	assert(N->ref != 0);
#endif

	if (N->ref == 1) {
	    N->ref = 0;
	    table->dead++;
#ifdef DD_STATS
	    table->nodesDropped++;
#endif
	    if (cuddIsConstant(N)) {
		table->constants.dead++;
		N = stack[--SP];
	    } else {
		ord = table->perm[N->index];
		stack[SP++] = Cudd_Regular(cuddE(N));
		table->subtables[ord].dead++;
		N = cuddT(N);
	    }
	} else {
	    cuddSatDec(N->ref);
	    N = stack[--SP];
	}
    } while (SP != 0);

} /* end of Cudd_RecursiveDeref */


/**Function********************************************************************

  Synopsis    [Decreases the reference count of BDD node n.]

  Description [Decreases the reference count of node n. If n dies,
  recursively decreases the reference counts of its children.  It is
  used to dispose of a BDD that is no longer needed. It is more
  efficient than Cudd_RecursiveDeref, but it cannot be used on
  ADDs. The greater efficiency comes from being able to assume that no
  constant node will ever die as a result of a call to this
  procedure.]

  SideEffects [None]

  SeeAlso     [Cudd_RecursiveDeref Cudd_DelayedDerefBdd]

******************************************************************************/
void
Cudd_IterDerefBdd(
  DdManager * table,
  DdNode * n)
{
    DdNode *N;
    int ord;
    DdNodePtr *stack = table->stack;
    int SP = 1;

    unsigned int live = table->keys - table->dead;
    if (live > table->peakLiveNodes) {
	table->peakLiveNodes = live;
    }

    N = Cudd_Regular(n);

    do {
#ifdef DD_DEBUG
	assert(N->ref != 0);
#endif

	if (N->ref == 1) {
	    N->ref = 0;
	    table->dead++;
#ifdef DD_STATS
	    table->nodesDropped++;
#endif
	    ord = table->perm[N->index];
	    stack[SP++] = Cudd_Regular(cuddE(N));
	    table->subtables[ord].dead++;
	    N = cuddT(N);
	} else {
	    cuddSatDec(N->ref);
	    N = stack[--SP];
	}
    } while (SP != 0);

} /* end of Cudd_IterDerefBdd */


/**Function********************************************************************

  Synopsis    [Decreases the reference count of BDD node n.]

  Description [Enqueues node n for later dereferencing. If the queue
  is full decreases the reference count of the oldest node N to make
  room for n. If N dies, recursively decreases the reference counts of
  its children.  It is used to dispose of a BDD that is currently not
  needed, but may be useful again in the near future. The dereferencing
  proper is done as in Cudd_IterDerefBdd.]

  SideEffects [None]

  SeeAlso     [Cudd_RecursiveDeref Cudd_IterDerefBdd]

******************************************************************************/
void
Cudd_DelayedDerefBdd(
  DdManager * table,
  DdNode * n)
{
    DdNode *N;
    int ord;
    DdNodePtr *stack;
    int SP;

    unsigned int live = table->keys - table->dead;
    if (live > table->peakLiveNodes) {
	table->peakLiveNodes = live;
    }

    n = Cudd_Regular(n);
#ifdef DD_DEBUG
    assert(n->ref != 0);
#endif

#ifdef DD_NO_DEATH_ROW
    N = n;
#else
    if (cuddIsConstant(n) || n->ref > 1) {
#ifdef DD_DEBUG
	assert(n->ref != 1 && (!cuddIsConstant(n) || n == DD_ONE(table)));
#endif
	cuddSatDec(n->ref);
	return;
    }

    N = table->deathRow[table->nextDead];

    if (N != NULL) {
#endif
#ifdef DD_DEBUG
	assert(!Cudd_IsComplement(N));
#endif
	stack = table->stack;
	SP = 1;
	do {
#ifdef DD_DEBUG
	    assert(N->ref != 0);
#endif
	    if (N->ref == 1) {
		N->ref = 0;
		table->dead++;
#ifdef DD_STATS
		table->nodesDropped++;
#endif
		ord = table->perm[N->index];
		stack[SP++] = Cudd_Regular(cuddE(N));
		table->subtables[ord].dead++;
		N = cuddT(N);
	    } else {
		cuddSatDec(N->ref);
		N = stack[--SP];
	    }
	} while (SP != 0);
#ifndef DD_NO_DEATH_ROW
    }
    table->deathRow[table->nextDead] = n;

    /* Udate insertion point. */
    table->nextDead++;
    table->nextDead &= table->deadMask;
#if 0
    if (table->nextDead == table->deathRowDepth) {
	if (table->deathRowDepth < table->looseUpTo / 2) {
	    extern void (*MMoutOfMemory)(long);
	    void (*saveHandler)(long) = MMoutOfMemory;
	    DdNodePtr *newRow;
	    MMoutOfMemory = Cudd_OutOfMem;
	    newRow = REALLOC(DdNodePtr,table->deathRow,2*table->deathRowDepth);
	    MMoutOfMemory = saveHandler;
	    if (newRow == NULL) {
		table->nextDead = 0;
	    } else {
		int i;
		table->memused += table->deathRowDepth;
		i = table->deathRowDepth;
		table->deathRowDepth <<= 1;
		for (; i < table->deathRowDepth; i++) {
		    newRow[i] = NULL;
		}
		table->deadMask = table->deathRowDepth - 1;
		table->deathRow = newRow;
	    }
	} else {
	    table->nextDead = 0;
	}
    }
#endif
#endif

} /* end of Cudd_DelayedDerefBdd */


/**Function********************************************************************

  Synopsis    [Decreases the reference count of ZDD node n.]

  Description [Decreases the reference count of ZDD node n. If n dies,
  recursively decreases the reference counts of its children.  It is
  used to dispose of a ZDD that is no longer needed.]

  SideEffects [None]

  SeeAlso     [Cudd_Deref Cudd_Ref Cudd_RecursiveDeref]

******************************************************************************/
void
Cudd_RecursiveDerefZdd(
  DdManager * table,
  DdNode * n)
{
    DdNode *N;
    int ord;
    DdNodePtr *stack = table->stack;
    int SP = 1;

    N = n;

    do {
#ifdef DD_DEBUG
	assert(N->ref != 0);
#endif

	cuddSatDec(N->ref);
    
	if (N->ref == 0) {
	    table->deadZ++;
#ifdef DD_STATS
	    table->nodesDropped++;
#endif
#ifdef DD_DEBUG
	    assert(!cuddIsConstant(N));
#endif
	    ord = table->permZ[N->index];
	    stack[SP++] = cuddE(N);
	    table->subtableZ[ord].dead++;
	    N = cuddT(N);
	} else {
	    N = stack[--SP];
	}
    } while (SP != 0);

} /* end of Cudd_RecursiveDerefZdd */


/**Function********************************************************************

  Synopsis    [Decreases the reference count of node.]

  Description [Decreases the reference count of node. It is primarily
  used in recursive procedures to decrease the ref count of a result
  node before returning it. This accomplishes the goal of removing the
  protection applied by a previous Cudd_Ref.]

  SideEffects [None]

  SeeAlso     [Cudd_RecursiveDeref Cudd_RecursiveDerefZdd Cudd_Ref]

******************************************************************************/
void
Cudd_Deref(
  DdNode * node)
{
    node = Cudd_Regular(node);
    cuddSatDec(node->ref);

} /* end of Cudd_Deref */


/**Function********************************************************************

  Synopsis [Checks the unique table for nodes with non-zero reference
  counts.]

  Description [Checks the unique table for nodes with non-zero
  reference counts. It is normally called before Cudd_Quit to make sure
  that there are no memory leaks due to missing Cudd_RecursiveDeref's.
  Takes into account that reference counts may saturate and that the
  basic constants and the projection functions are referenced by the
  manager.  Returns the number of nodes with non-zero reference count.
  (Except for the cases mentioned above.)]

  SideEffects [None]

  SeeAlso     []

******************************************************************************/
int
Cudd_CheckZeroRef(
  DdManager * manager)
{
    int size;
    int i, j;
    int remain;	/* the expected number of remaining references to one */
    DdNodePtr *nodelist;
    DdNode *node;
    DdNode *sentinel = &(manager->sentinel);
    DdSubtable *subtable;
    int count = 0;
    int index;

#ifndef DD_NO_DEATH_ROW
    cuddClearDeathRow(manager);
#endif

    /* First look at the BDD/ADD subtables. */
    remain = 1; /* reference from the manager */
    size = manager->size;
    remain += 2 * size;	/* reference from the BDD projection functions */

    for (i = 0; i < size; i++) {
	subtable = &(manager->subtables[i]);
	nodelist = subtable->nodelist;
	for (j = 0; (unsigned) j < subtable->slots; j++) {
	    node = nodelist[j];
	    while (node != sentinel) {
		if (node->ref != 0 && node->ref != DD_MAXREF) {
		    index = (int) node->index;
		    if (node != manager->vars[index]) {
			count++;
		    } else {
			if (node->ref != 1) {
			    count++;
			}
		    }
		}
		node = node->next;
	    }
	}
    }

    /* Then look at the ZDD subtables. */
    size = manager->sizeZ;
    if (size) /* references from ZDD universe */
	remain += 2;

    for (i = 0; i < size; i++) {
	subtable = &(manager->subtableZ[i]);
	nodelist = subtable->nodelist;
	for (j = 0; (unsigned) j < subtable->slots; j++) {
	    node = nodelist[j];
	    while (node != NULL) {
		if (node->ref != 0 && node->ref != DD_MAXREF) {
		    index = (int) node->index;
		    if (node == manager->univ[manager->permZ[index]]) {
			if (node->ref > 2) {
			    count++;
			}
		    } else {
			count++;
		    }
		}
		node = node->next;
	    }
	}
    }

    /* Now examine the constant table. Plusinfinity, minusinfinity, and
    ** zero are referenced by the manager. One is referenced by the
    ** manager, by the ZDD universe, and by all projection functions.
    ** All other nodes should have no references.
    */
    nodelist = manager->constants.nodelist;
    for (j = 0; (unsigned) j < manager->constants.slots; j++) {
	node = nodelist[j];
	while (node != NULL) {
	    if (node->ref != 0 && node->ref != DD_MAXREF) {
		if (node == manager->one) {
		    if ((int) node->ref != remain) {
			count++;
		    }
		} else if (node == manager->zero ||
		node == manager->plusinfinity ||
		node == manager->minusinfinity) {
		    if (node->ref != 1) {
			count++;
		    }
		} else {
		    count++;
		}
	    }
	    node = node->next;
	}
    }
    return(count);

} /* end of Cudd_CheckZeroRef */


/*---------------------------------------------------------------------------*/
/* Definition of internal functions                                          */
/*---------------------------------------------------------------------------*/


/**Function********************************************************************

  Synopsis    [Brings children of a dead node back.]

  Description []

  SideEffects [None]

  SeeAlso     [cuddReclaimZdd]

******************************************************************************/
void
cuddReclaim(
  DdManager * table,
  DdNode * n)
{
    DdNode *N;
    int ord;
    DdNodePtr *stack = table->stack;
    int SP = 1;
    double initialDead = table->dead;

    N = Cudd_Regular(n);

#ifdef DD_DEBUG
    assert(N->ref == 0);
#endif

    do {
	if (N->ref == 0) {
	    N->ref = 1;
	    table->dead--;
	    if (cuddIsConstant(N)) {
		table->constants.dead--;
		N = stack[--SP];
	    } else {
		ord = table->perm[N->index];
		stack[SP++] = Cudd_Regular(cuddE(N));
		table->subtables[ord].dead--;
		N = cuddT(N);
	    }
	} else {
	    cuddSatInc(N->ref);
	    N = stack[--SP];
	}
    } while (SP != 0);

    N = Cudd_Regular(n);
    cuddSatDec(N->ref);
    table->reclaimed += initialDead - table->dead;

} /* end of cuddReclaim */


/**Function********************************************************************

  Synopsis    [Brings children of a dead ZDD node back.]

  Description []

  SideEffects [None]

  SeeAlso     [cuddReclaim]

******************************************************************************/
void
cuddReclaimZdd(
  DdManager * table,
  DdNode * n)
{
    DdNode *N;
    int ord;
    DdNodePtr *stack = table->stack;
    int SP = 1;

    N = n;

#ifdef DD_DEBUG
    assert(N->ref == 0);
#endif

    do {
	cuddSatInc(N->ref);

	if (N->ref == 1) {
	    table->deadZ--;
	    table->reclaimed++;
#ifdef DD_DEBUG
	    assert(!cuddIsConstant(N));
#endif
	    ord = table->permZ[N->index];
	    stack[SP++] = cuddE(N);
	    table->subtableZ[ord].dead--;
	    N = cuddT(N);
	} else {
	    N = stack[--SP];
	}
    } while (SP != 0);

    cuddSatDec(n->ref);

} /* end of cuddReclaimZdd */


/**Function********************************************************************

  Synopsis    [Shrinks the death row.]

  Description [Shrinks the death row by a factor of four.]

  SideEffects [None]

  SeeAlso     [cuddClearDeathRow]

******************************************************************************/
void
cuddShrinkDeathRow(
  DdManager *table)
{
#ifndef DD_NO_DEATH_ROW
    int i;

    if (table->deathRowDepth > 3) {
	for (i = table->deathRowDepth/4; i < table->deathRowDepth; i++) {
	    if (table->deathRow[i] == NULL) break;
	    Cudd_IterDerefBdd(table,table->deathRow[i]);
	    table->deathRow[i] = NULL;
	}
	table->deathRowDepth /= 4;
	table->deadMask = table->deathRowDepth - 1;
	if ((unsigned) table->nextDead > table->deadMask) {
	    table->nextDead = 0;
	}
	table->deathRow = REALLOC(DdNodePtr, table->deathRow,
				   table->deathRowDepth);
    }
#endif

} /* end of cuddShrinkDeathRow */


/**Function********************************************************************

  Synopsis    [Clears the death row.]

  Description []

  SideEffects [None]

  SeeAlso     [Cudd_DelayedDerefBdd Cudd_IterDerefBdd Cudd_CheckZeroRef
  cuddGarbageCollect]

******************************************************************************/
void
cuddClearDeathRow(
  DdManager *table)
{
#ifndef DD_NO_DEATH_ROW
    int i;

    for (i = 0; i < table->deathRowDepth; i++) {
	if (table->deathRow[i] == NULL) break;
	Cudd_IterDerefBdd(table,table->deathRow[i]);
	table->deathRow[i] = NULL;
    }
#ifdef DD_DEBUG
    for (; i < table->deathRowDepth; i++) {
	assert(table->deathRow[i] == NULL);
    }
#endif
    table->nextDead = 0;
#endif

} /* end of cuddClearDeathRow */


/**Function********************************************************************

  Synopsis    [Checks whether a node is in the death row.]

  Description [Checks whether a node is in the death row. Returns the
  position of the first occurrence if the node is present; -1
  otherwise.]

  SideEffects [None]

  SeeAlso     [Cudd_DelayedDerefBdd cuddClearDeathRow]

******************************************************************************/
int
cuddIsInDeathRow(
  DdManager *dd,
  DdNode *f)
{
#ifndef DD_NO_DEATH_ROW
    int i;

    for (i = 0; i < dd->deathRowDepth; i++) {
	if (f == dd->deathRow[i]) {
	    return(i);
	}
    }
#endif

    return(-1);

} /* end of cuddIsInDeathRow */


/**Function********************************************************************

  Synopsis    [Counts how many times a node is in the death row.]

  Description []

  SideEffects [None]

  SeeAlso     [Cudd_DelayedDerefBdd cuddClearDeathRow cuddIsInDeathRow]

******************************************************************************/
int
cuddTimesInDeathRow(
  DdManager *dd,
  DdNode *f)
{
    int count = 0;
#ifndef DD_NO_DEATH_ROW
    int i;

    for (i = 0; i < dd->deathRowDepth; i++) {
	count += f == dd->deathRow[i];
    }
#endif

    return(count);

} /* end of cuddTimesInDeathRow */

/*---------------------------------------------------------------------------*/
/* Definition of static functions                                            */
/*---------------------------------------------------------------------------*/
