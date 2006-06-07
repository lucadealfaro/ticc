/*
 * $Id: graph_static_int.h,v 1.1 2005/04/21 05:58:02 luca Exp $
 *
 */

#include "graph_static.h"

typedef struct g_field_struct {
    int num_g_slots;
    int num_v_slots;
    int num_e_slots;
    gGeneric user_data;
} g_field_t;
