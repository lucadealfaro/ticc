/*
 * $Id: graph_static.h,v 1.1 2005/04/21 05:58:02 luca Exp $
 *
 */

/******************************* graph_static.h ************************/

EXTERN graph_t *g_alloc_static ARGS((int, int, int));
EXTERN void g_free_static ARGS((graph_t *, void (*)(gGeneric), void (*)(gGeneric), void (*)(gGeneric)));
EXTERN graph_t *g_dup_static ARGS((graph_t *, char *(*)(gGeneric), char *(*)(gGeneric), char *(*)(gGeneric)));
EXTERN void g_set_g_slot_static ARGS((graph_t *, int, char *));
EXTERN char *g_get_g_slot_static ARGS((graph_t *, int));
EXTERN void g_copy_g_slots_static ARGS((graph_t *, graph_t *, char *(*)(gGeneric)));
EXTERN vertex_t *g_add_vertex_static ARGS((graph_t *));
EXTERN void g_delete_vertex_static ARGS((vertex_t *, void (*)(gGeneric), void (*)(gGeneric)));
EXTERN void g_set_v_slot_static ARGS((vertex_t *, int, char *));
EXTERN char *g_get_v_slot_static ARGS((vertex_t *, int));
EXTERN void g_copy_v_slots_static ARGS((vertex_t *, vertex_t *, char *(*)(gGeneric)));
EXTERN edge_t *g_add_edge_static ARGS((vertex_t *, vertex_t *));
EXTERN void g_delete_edge_static ARGS((edge_t *, void (*)(gGeneric)));
EXTERN void g_set_e_slot_static ARGS((edge_t *, int, char *));
EXTERN char *g_get_e_slot_static ARGS((edge_t *, int));
EXTERN void g_copy_e_slots_static ARGS((edge_t *, edge_t *, char *(*)(gGeneric)));
