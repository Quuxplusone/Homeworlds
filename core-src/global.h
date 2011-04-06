
#ifndef H_GLOBAL
 #define H_GLOBAL

#include <assert.h>
#include <string>

#define NUMPLAYERS 2

enum Color { RED=0, YELLOW=1, GREEN=2, BLUE=3, UNKNOWN_COLOR=4 };
enum Size { SMALL=0, MEDIUM=1, LARGE=2, UNKNOWN_SIZE=3 };

inline Color& operator ++ (Color &c) { c = (Color)((int)c + 1); return c; }
inline Size& operator ++ (Size &s) { s = (Size)((int)s + 1); return s; }


inline const char *piece2str(Color c, Size s)
{
    static const char tab[5][4][3] = {
        { "r1", "r2", "r3", "r" },
        { "y1", "y2", "y3", "y" },
        { "g1", "g2", "g3", "g" },
        { "b1", "b2", "b3", "b" },
        { "1",  "2",  "3",  "" }
    };
    return tab[c][s];
}


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

extern std::string mprintf(const char *, ...);

#endif /* H_GLOBAL */
