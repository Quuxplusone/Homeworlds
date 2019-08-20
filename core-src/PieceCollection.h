#pragma once

#include "global.h"
#include <string>

struct Piece {
    Color color : 4;
    Size size : 4;

    explicit Piece() : color(UNKNOWN_COLOR), size(UNKNOWN_SIZE) {}
    explicit Piece(Color c, Size s) : color(c), size(s) {}

    const char *toString() const {
        static const char tab[5][4][3] = {
            { "r1", "r2", "r3", "r" },
            { "y1", "y2", "y3", "y" },
            { "g1", "g2", "g3", "g" },
            { "b1", "b2", "b3", "b" },
            { "1",  "2",  "3",  "" }
        };
        return tab[color][size];
    }

    bool sanitycheck() const {
        return (color == RED || color == YELLOW || color == GREEN || color == BLUE || color == UNKNOWN_COLOR) &&
               (size == SMALL || size == MEDIUM || size == LARGE || size == UNKNOWN_SIZE);
    }

    bool empty() const {
        return (color == UNKNOWN_COLOR && size == UNKNOWN_SIZE);
    }

    bool isMissingPieces() const {
        return (color == UNKNOWN_COLOR || size == UNKNOWN_SIZE);
    }
};

class PieceCollection {
    signed char pieces[4][3];

public:
    enum { MAXSTRLEN = 24*(NUMPLAYERS+1) };

    explicit PieceCollection();

    bool contains(const PieceCollection &) const;
    bool operator == (const PieceCollection &) const;
    bool operator != (const PieceCollection &rhs) const { return !(*this == rhs); }

    int empty() const { return !numberOf(RED) && !numberOf(YELLOW) && !numberOf(GREEN) && !numberOf(BLUE); }
    int number() const { return numberOf(SMALL)+numberOf(MEDIUM)+numberOf(LARGE); }
    int numberOf(Color c) const { return pieces[c][SMALL]+pieces[c][MEDIUM]+pieces[c][LARGE]; }
    int numberOf(Size s) const { return pieces[RED][s]+pieces[YELLOW][s]
                                       +pieces[GREEN][s]+pieces[BLUE][s]; }
    int numberOf(Color c, Size s) const { return pieces[c][s]; }
    int numberOf(Piece p) const { return pieces[p.color][p.size]; }
    int numberAtLeast(Size s) const {
        if (s == SMALL) return number();
        else if (s == MEDIUM) return numberOf(MEDIUM) + numberOf(LARGE);
        else return numberOf(LARGE);
    }
    Size smallestSizeOf(Color c) const;
    Size biggestSize() const { return numberOf(LARGE)? LARGE: numberOf(MEDIUM)? MEDIUM: SMALL; }
    Size biggestSizeOf(Color c) const { return numberOf(c, LARGE)? LARGE: numberOf(c, MEDIUM)? MEDIUM: SMALL; }
    bool isAdjacentTo(const PieceCollection &that) const {
        for (Size s = SMALL; s <= LARGE; ++s) {
            if (this->numberOf(s) != 0 && that.numberOf(s) != 0) {
                return false;
            }
        }
        return true;
    }

    void clear();
    void insert(Piece p) { pieces[p.color][p.size] += 1; }
    void insert(Color c, Size s) { pieces[c][s] += 1; }
    void insert(Color c, Size s, int count) { pieces[c][s] += count; }
    void remove(Piece p) { pieces[p.color][p.size] -= 1; }
    void remove(Color c, Size s) { pieces[c][s] -= 1; }
    void removeAll(Color c) { pieces[c][SMALL] = 0; pieces[c][MEDIUM] = 0; pieces[c][LARGE] = 0; }
    void removeAll(Size s) { pieces[RED][s] = 0; pieces[YELLOW][s] = 0; pieces[GREEN][s] = 0; pieces[BLUE][s] = 0; }
    void operator+=(const PieceCollection&);
    void operator-=(const PieceCollection&);
    void operator+=(Piece p) { pieces[p.color][p.size] += 1; }
    void operator-=(Piece p) { pieces[p.color][p.size] -= 1; }

    std::string toString() const;
    char *toString(char buffer[MAXSTRLEN+1]) const;
    char *toComparableString(char buffer[MAXSTRLEN+1], unsigned char mask) const;
    bool scan(const char *text);
};
