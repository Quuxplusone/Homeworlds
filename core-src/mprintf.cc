#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include "mprintf.h"

std::string mprintf(const char *fmt, ...)
{
    std::string result;
    va_list ap;
    va_start(ap, fmt);
    static char buffer[1000];
    int wanted_space = vsnprintf(buffer, sizeof buffer, fmt, ap);
    va_end(ap);
    if (wanted_space > (int)(sizeof buffer)) {
        char *newbuf = (char *)malloc(wanted_space+1);
        assert(newbuf != nullptr);
        va_start(ap, fmt);
        int written = vsnprintf(newbuf, wanted_space+1, fmt, ap);
        assert(written == wanted_space);
        (void)written;
        va_end(ap);
        result = newbuf;
        free(newbuf);
    } else {
        result = buffer;
    }
    return result;
}
