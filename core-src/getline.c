
/*
   This is the implementation of the |getline| library.

   Last modified 24 January 2006 by Arthur O'Dwyer.
   Public domain.
*/

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "getline.h"

static void try_resize(char **p, size_t *cap);
static char *try_exact_fit(char **p, size_t len);


/*
   The |fgetline_notrim| function reads characters from |stream| and
   stores them into a dynamically allocated buffer pointed to by |*s|.
   (The old value of |*s| is ignored by this function.)
   Reading stops after an |EOF| or a newline. If a newline is read,
   it is stored into the buffer.

   |fgetline_notrim| returns |p| on success; and |NULL| on I/O error,
   or when end-of-file occurs while no characters have been read,
   or when the line length exceeds |(size_t)-1|,
   or when a call to |malloc| returns |NULL|. In all four cases,
   the characters which have been read so far, if any, still
   reside in the dynamically allocated buffer |*p|.
*/
char *fgetline_notrim(char **p, FILE *stream)
{
    size_t cap = 0;
    size_t len = 0;
    char *rc;

    *p = NULL;

    while (1) {
        try_resize(p, &cap);
        if (cap-1 <= len) {
            /* |try_resize| failed */
            return NULL;
        }
        rc = fgets(*p+len, cap-len, stream);
        if (rc == NULL) {
            /*
               EOF or input error. In either case, once |NULL| has
               been returned, the contents of the buffer are unusable.
            */
            (*p)[len] = '\0';
            try_exact_fit(p, len);
            if (feof(stream) && len > 0)
              return *p;
            else return NULL;
        }
        else if (strchr(*p+len, '\n') != NULL) {
            /* a newline has been read */
            return try_exact_fit(p, len+strlen(*p+len));
        }
        else {
            /* we must continue reading */
            len += strlen(*p+len);
            if (feof(stream))
              return try_exact_fit(p, len);
        }
    }
}


/*
   The |try_resize| function tries to resize |*p| so it can hold more
   data. It will always yield a valid, consistent |*p| and |*cap| ---
   |*cap| will never decrease, and no data will be lost from |*p|.
   But if a call to |realloc| fails, |*cap| will be unchanged.
   One important thing to notice: We must never increase |*cap| by
   more than |INT_MAX|, since the second parameter to |fgets| is of
   type |int|.
*/
static void try_resize(char **p, size_t *cap)
{
    /*
       We aren't expecting any really long lines, here. But if the
       current line has exceeded 500 characters, there's probably
       something special going on (like an attempted buffer overflow
       attack), and we'll start increasing the buffer capacity
       geometrically.
    */
    size_t newcap = (*cap < 500)? (*cap + 16):
                    (*cap/2 < INT_MAX)? (*cap + *cap/2):
                    (*cap + INT_MAX);
    char *newp;
    if (newcap < *cap) {
        /* The line length has exceeded |(size_t)-1|. Wow! */
        if (*cap == (size_t)-1) return;
        else newcap = (size_t)-1;
    }

    newp = realloc(*p, newcap);
    /* Maybe we can't get that much memory. Try smaller chunks. */
    while (newp == NULL) {
        newcap = *cap + (newcap - *cap)/2;
        if (newcap == *cap) break;
        newp = realloc(*p, newcap);
    }

    if (newp != NULL)
      *p = newp;

    /* At this point, |*p| hasn't lost any data, and |newcap| is valid. */
    *cap = newcap;
    return;
}


/*
   The |try_exact_fit| function tries to resize the given buffer to
   exactly the right size to fit its contents, counting the null
   terminator. If that fails, we just return the original buffer.
*/
static char *try_exact_fit(char **p, size_t len)
{
    char *r = realloc(*p, len+1);
    if (r != NULL) *p = r;
    return *p;
}


/*
   The function |trim_113| just slaps a zero byte into the position
   after the last non-whitespace character in the given string. It
   is used to strip off newlines from the results of |fgetline_113|
   and |getline_113|. You can also call it explicitly on the output
   of |getline_notrim|, or any other string, for that matter.

   If the parameter is |NULL|, this function will return |NULL|.
   Otherwise, the zero byte is written and the resulting string is
   returned.
*/
char *trim_113(char *s)
{
    char *end;
    if (s == NULL)
      return NULL;
    end = strchr(s, '\0');
    while (end > s && isspace((unsigned char)end[-1]))
      --end;
    *end = '\0';
    return s;
}


/*
   The |fgetline_113| function is just like |fgetline_notrim|, except
   that it calls |trim_113| on the resulting string before returning.
*/
char *fgetline_113(char **line, FILE *fp)
{
    char *rc = fgetline_notrim(line, fp);
    trim_113(*line);
    return rc;
}


/*
   The |getline_notrim| function is the same as the |fgetline_notrim|
   function, except that |stream| is given the value of |stdin|.
*/
char *getline_notrim(char **p)
{
    return fgetline_notrim(p, stdin);
}


/*
   The |getline_113| function is the same as the |fgetline_113|
   function, except that |stream| is given the value of |stdin|.
*/
char *getline_113(char **p)
{
    return fgetline_113(p, stdin);
}
