/*
 * Revision Control Information
 *
 * $Id: tmpfile.c,v 1.1 2005/04/21 05:58:02 luca Exp $
 *
 */

/*
 *  util_tmpfile -- open an unnamed temporary file
 *
 *  Many compilers/systems do not have this, or have buggy versions.
 *
 */

/* LINTLIBRARY */

/* util_tempnam and check_directory are from
   Jonathan I. Kamens          <jik@pit-manager.mit.edu> */

/* modified slightly by Ellen Sentovich ellen@ic.berkeley.edu */

#include <sys/types.h>
#if HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#if HAVE_SYS_FILE_H
#  include <sys/file.h>
#endif

#include "util.h"

static char check_directory(char *dir)
{
     struct stat statbuf;

     if (! dir)
         return 0;
     else if (stat(dir, &statbuf) < 0)
         return 0;
     else if ((statbuf.st_mode & S_IFMT) != S_IFDIR)
         return 0;
     else if (access(dir, W_OK | X_OK) < 0)
         return 0;
     else
         return 1;
}

/* function for creating temporary filenames */
char *util_tempnam(char *dir, char *pfx)
{
     extern char *getenv(const char *);
     char *tmpdir = NULL, *env, *filename;
     static char unique_letters[4] = "AAA";
     char addslash = 0;

     /*
      * If a directory is passed in, verify that it exists and is a
      * directory and is writeable by this process.  If no directory
      * is passed in, or if the directory that is passed in does not
      * exist, check the environment variable TMPDIR.  If it isn't
      * set, check the predefined constant P_tmpdir.  If that isn't
      * set, use "/tmp/".
      */

     if ((env = getenv ("TMPDIR")) && check_directory(env))
         tmpdir = env;
     else if (dir && check_directory(dir))
         tmpdir = dir;
#ifdef P_tmpdir
     else if (check_directory(P_tmpdir))
         tmpdir = P_tmpdir;
#endif
     else
         tmpdir = "/tmp/";

     /*
      * OK, now that we've got a directory, figure out whether or not
      * there's a slash at the end of it.
      */
     if (tmpdir[strlen(tmpdir) - 1] != '/')
         addslash = 1;

     /*
      * Now figure out the set of unique letters.
      */
     unique_letters[0]++;
     if (unique_letters[0] > 'Z') {
         unique_letters[0] = 'A';
         unique_letters[1]++;
         if (unique_letters[1] > 'Z') {
             unique_letters[1] = 'A';
             unique_letters[2]++;
             if (unique_letters[2] > 'Z') {
                 unique_letters[2]++;
             }
         }
     }

     /*
      * Allocate a string of sufficient length.
      */
     if (pfx) {
         filename = (char *) malloc(strlen(tmpdir) + addslash + strlen(pfx) + 10
);
     } else {
         filename = (char *) malloc(strlen(tmpdir) + addslash + 10);
     }

     /*
      * And create the string.
      */
     (void) sprintf(filename, "%s%s%s%sa%05d", tmpdir, addslash ? "/" : "",
                    pfx ? pfx : "", unique_letters, (int)getpid());

     return filename;
}


#ifdef UNIX

FILE *
util_tmpfile(void)
{
    FILE *fp;
    char *filename;

    filename = util_tempnam(NIL(char), "VIS");
    if ((fp = fopen(filename, "w+")) == NULL) {
	FREE(filename);
	return NULL;
    }
    (void) unlink(filename); 
    FREE(filename);
    return fp;
}

#else

FILE *
util_tmpfile(void)
{
    FILE *fp;

    if ((fp = fopen("utiltmp", "w+")) == NULL) {
	return NULL;
    }
    (void) unlink("utiltmp");
    return fp;
}

#endif
