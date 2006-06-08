#include "mdd.h"

/*
 * MDD Package
 *
 * $Id: mdd_quit.c,v 1.1 2005/04/21 05:58:02 luca Exp $
 *
 * Author: Timothy Kam
 *
 * Copyright 1992 by the Regents of the University of California.
 *
 * All rights reserved.  Permission to use, copy, modify and distribute
 * this software is hereby granted, provided that the above copyright
 * notice and this permission notice appear in all copies.  This software
 * is made available as is, with no warranties.
 */

void
mdd_quit(mdd_manager *mgr)
{
    int i;
    mvar_type one_mvar_struct;
    bvar_type one_bvar_struct;
    bdd_external_hooks *hook;  
    array_t *mvar_list = mdd_ret_mvar_list(mgr);
    array_t *bvar_list = mdd_ret_bvar_list(mgr);


    for (i=0; i<array_n(mvar_list); i++) {
	one_mvar_struct = array_fetch(mvar_type, mvar_list, i);
        FREE(one_mvar_struct.name);
        FREE(one_mvar_struct.encoding);
	array_free(one_mvar_struct.bvars);
    }
    array_free(mvar_list); 
    for (i=0; i<array_n(bvar_list); i++) {
	one_bvar_struct = array_fetch(bvar_type, bvar_list, i);
        mdd_free(one_bvar_struct.node);
    }
    array_free(bvar_list); 

    hook = bdd_get_external_hooks(mgr);
    FREE(hook->mdd);

    bdd_end(mgr);
}

/*---------------------------------------------------------------------------*/
/* Static function prototypes                                                */
/*---------------------------------------------------------------------------*/

