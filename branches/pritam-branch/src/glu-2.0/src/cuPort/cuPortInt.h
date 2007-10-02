/**CHeaderFile*****************************************************************

  FileName    [cuddPortInt.h]

  PackageName [cu_port]

  Synopsis    [Header file used by cu_port.]

  Author      [Abelardo Pardo <abel@vlsi.colorado.edu>]

  Copyright   [This file was created at the University of Colorado at Boulder.
  The University of Colorado at Boulder makes no warranty about the suitability
  of this software for any purpose.  It is presented on an AS IS basis.]

  Revision    [$Id: cuPortInt.h,v 1.1 2005/04/21 05:58:01 luca Exp $]

******************************************************************************/

#ifndef _CU_PORT_INT
#define _CU_PORT_INT

#include "util.h"
#include "array.h"
#include "st.h"

#include "cuddInt.h"
#include "bdd.h"

/*---------------------------------------------------------------------------*/
/* Constant declarations                                                     */
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
/* Type declarations                                                         */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Structure declarations                                                    */
/*---------------------------------------------------------------------------*/
struct bdd_t {
  boolean free; /* TRUE if this is free, FALSE otherwise ... */
  DdNode *node; /* ptr to the top node of the function */
  DdManager *mgr; /* the manager */
};


/*---------------------------------------------------------------------------*/
/* Variable declarations                                                     */
/*---------------------------------------------------------------------------*/


/*---------------------------------------------------------------------------*/
/* Macro declarations                                                        */
/*---------------------------------------------------------------------------*/


/**AutomaticStart*************************************************************/

/*---------------------------------------------------------------------------*/
/* Function prototypes                                                       */
/*---------------------------------------------------------------------------*/


/**AutomaticEnd***************************************************************/

#endif /* _CU_PORT_INT */

