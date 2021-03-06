#pragma once

#include <assert.h>

/* Variables that are used only in assertions should use this. */
#ifdef NDEBUG
 #define UNUSED(varname) __attribute__((unused)) varname
#else
 #define UNUSED(varname) varname
#endif /* NDEBUG */

constexpr int NUMPLAYERS = 2;

enum Color { RED=0, YELLOW=1, GREEN=2, BLUE=3, UNKNOWN_COLOR=4 };
enum Size { SMALL=0, MEDIUM=1, LARGE=2, UNKNOWN_SIZE=3 };

inline Color& operator ++ (Color &c) { c = (Color)((int)c + 1); return c; }
inline Size& operator ++ (Size &s) { s = (Size)((int)s + 1); return s; }

inline const char *color2str(Color c)
{
    switch (c) {
        case RED: return "red";
        case YELLOW: return "yellow";
        case GREEN: return "green";
        case BLUE: return "blue";
        case UNKNOWN_COLOR: return "";
    }
    /*NOTREACHED*/
    assert(false);
    return "";
}
