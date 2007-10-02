/*
 * $Id: error.h,v 1.1 2005/04/21 05:58:01 luca Exp $
 *
 */
EXTERN void error_init ARGS((void));
EXTERN void error_append ARGS((char *));
EXTERN char *error_string ARGS((void));
EXTERN void error_cleanup ARGS((void));
