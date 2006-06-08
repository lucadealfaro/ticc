/**CFile***********************************************************************

  FileName    [heap.h]

  PackageName [heap]

  Synopsis    [Heap-based priority queue.]

  Description [This is the external header file for the heap-based
  priority queue.  The priority of each item is determined by an
  integer key.  The first element of the heap is the one with the
  smallest key.  Multiple items with the same key can be inserted.
  Refer to Chapter 7 of Cormen, Leiserson, and Rivest for the theory.
  (The only significant difference is that the array indices start
  from 0 in this implementation.)]

  SeeAlso     []

  Author      [Fabio Somenzi]

  Copyright   [This file was created at the University of Colorado at
  Boulder.  The University of Colorado at Boulder makes no warranty
  about the suitability of this software for any purpose.  It is
  presented on an AS IS basis.]

  Revision    [$Id: heap.h,v 1.1 2005/04/21 05:58:02 luca Exp $]

******************************************************************************/

#ifndef _HEAP
#define _HEAP

/*---------------------------------------------------------------------------*/
/* Nested includes                                                           */
/*---------------------------------------------------------------------------*/
#include "util.h"
#undef MAX
#undef MIN

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Stucture declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/

typedef struct HeapSlot HeapSlot_t;

typedef struct Heap Heap_t;

/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/

/**Macro***********************************************************************

  Synopsis    [Iterates over the elements of a heap.]

  SideEffects [none]

******************************************************************************/
#define Heap_HeapForEachItem(                                              \
  /* Heap_t * */ heap /* heap whose element should be enumerated */,       \
  /* int */      i    /* local variable for iterator */,                   \
  /* void * */	 data /* heap item */                                      \
)                                                                          \
  for((i) = 0; (((i) < (heap)->nslots) && (data = (heap)->slots[i].item)); \
      (i)++)



/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/

EXTERN Heap_t * Heap_HeapInit ARGS((int length));
EXTERN void Heap_HeapFree ARGS((Heap_t *heap));
EXTERN int Heap_HeapInsert ARGS((Heap_t *heap, void *item, int key));
EXTERN int Heap_HeapExtractMin ARGS((Heap_t *heap, void **item, int *key));
EXTERN int Heap_HeapCount ARGS((Heap_t *heap));
EXTERN Heap_t * Heap_HeapClone ARGS((Heap_t *source));
EXTERN int Heap_HeapTest ARGS((Heap_t *heap));

/**AutomaticEnd***************************************************************/

#endif /* _HEAP */
