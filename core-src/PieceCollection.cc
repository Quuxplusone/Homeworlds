
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "state.h"

PieceCollection::PieceCollection()
{
    this->clear();
}

void PieceCollection::clear()
{
    memset(this->pieces, 0x00, sizeof this->pieces);
}

Size PieceCollection::smallestSizeOf(Color c) const
{
    if (pieces[c][SMALL] > 0) return SMALL;
    if (pieces[c][MEDIUM] > 0) return MEDIUM;
    if (pieces[c][LARGE] > 0) return LARGE;
    /*NOTREACHED*/
    assert(false);
}

bool PieceCollection::contains(const PieceCollection &rhs) const
{
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            if (this->pieces[c][s] < rhs.pieces[c][s])
              return false;
        }
    }
    return true;
}

PieceCollection &PieceCollection::operator -= (const PieceCollection &rhs)
{
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            assert(this->pieces[c][s] >= rhs.pieces[c][s]);
            this->pieces[c][s] -= rhs.pieces[c][s];
        }
    }
    return *this;
}

PieceCollection &PieceCollection::operator += (const PieceCollection &rhs)
{
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            this->pieces[c][s] += rhs.pieces[c][s];
        }
    }
    return *this;
}

/* This function takes a char buffer, stores the representation of this
 * PieceCollection in it, and returns a pointer to the terminating '\0'.
 * This is useful in the common case that the caller wants to concatenate
 * something onto the end of the string. */
char *PieceCollection::toString(char *buffer) const
{
    assert(buffer != NULL);
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            for (int i=0; i < this->pieces[c][s]; ++i) {
                *buffer++ = "rygb"[c];
                *buffer++ = '1'+(int)s;
            }
        }
    }
    *buffer = '\0';
    return buffer;
}

std::string PieceCollection::toString() const
{
    static char buffer[MAXSTRLEN+1];
    (void)this->toString(buffer);
    return buffer;
}


/* Return true on success; return false on failure. */
bool PieceCollection::scan(const char *text)
{
    this->clear();
    /* An empty string is not a piece collection; an empty collection
     * is denoted by "-". */
    if (text[0] == '\0') {
        return false;
    } else if (text[0] == '-' && text[1] == '\0') {
        return true;
    }
    for (int i=0; text[i] != '\0'; ++i) {
        Color c;
        Size s;
        switch (text[i]) {
            case 'r': c = RED; break;
            case 'y': c = YELLOW; break;
            case 'g': c = GREEN; break;
            case 'b': c = BLUE; break;
            default: return false;
        }
        ++i;
        switch (text[i]) {
            case '1': s = SMALL; break;
            case '2': s = MEDIUM; break;
            case '3': s = LARGE; break;
            default: return false;
        }
        this->insert(c, s);
    }
    return true;
}

