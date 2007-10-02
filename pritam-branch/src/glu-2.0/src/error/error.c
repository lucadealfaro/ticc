/*
 * $Id: error.c,v 1.1 2005/04/21 05:58:01 luca Exp $
 *
 */
#include "util.h"
#include "error.h"

static char *error_str = 0;
static int error_str_len, error_str_maxlen;


void
error_init(void)
{
    if (error_str != 0) {
	FREE(error_str);
    }
    error_str_len = 0;
    error_str_maxlen = 100;
    error_str = ALLOC(char, error_str_maxlen);
    *error_str = '\0';
}


void
error_append(char *s)
{
    int slen;

    slen = strlen(s);
    if (error_str_len + slen + 1 > error_str_maxlen) {
	error_str_maxlen = (error_str_len + slen) * 2;	/* cstevens@ic */
	error_str = REALLOC(char, error_str, error_str_maxlen);
    }
    (void) strcpy(error_str + error_str_len, s);
    error_str_len += slen;
}


char *
error_string(void)
{
    return error_str;
}


void
error_cleanup(void)
{
    FREE(error_str);
    error_str_len = 0;
    error_str_maxlen = 0;
    error_str = 0;
}
