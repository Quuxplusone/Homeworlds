
#ifndef H_STATE
 #define H_STATE

#include <stdio.h>
#include <string>
#include <vector>
#include "global.h"


class PieceCollection {
    signed char pieces[4][3];

  public:
    enum { MAXSTRLEN = 24*(NUMPLAYERS+1) };

    PieceCollection();
    bool contains(const PieceCollection &rhs) const;
    int empty() const { return !numberOf(RED) && !numberOf(YELLOW) && !numberOf(GREEN) && !numberOf(BLUE); }
    int number() const { return numberOf(SMALL)+numberOf(MEDIUM)+numberOf(LARGE); }
    int numberOf(Color c) const { return pieces[c][SMALL]+pieces[c][MEDIUM]+pieces[c][LARGE]; }
    int numberOf(Size s) const { return pieces[RED][s]+pieces[YELLOW][s]
                                       +pieces[GREEN][s]+pieces[BLUE][s]; }
    int numberOf(Color c, Size s) const { return pieces[c][s]; }
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
            if (this->numberOf(s) != 0 && that.numberOf(s) != 0)
              return false;
        }
        return true;
    }
    
    void clear();
    void insert(Color c, Size s) { pieces[c][s] += 1; }
    void insert(Color c, Size s, int count) { pieces[c][s] += count; }
    void remove(Color c, Size s) { pieces[c][s] -= 1; }
    void removeAll(Color c) { pieces[c][SMALL] = 0; pieces[c][MEDIUM] = 0; pieces[c][LARGE] = 0; }
    void removeAll(Size s) { pieces[RED][s] = 0; pieces[YELLOW][s] = 0; pieces[GREEN][s] = 0; pieces[BLUE][s] = 0; }
    PieceCollection& operator += (const PieceCollection &);
    PieceCollection& operator -= (const PieceCollection &);
    
    std::string toString() const;
    char *toString(char buffer[MAXSTRLEN+1]) const;
    char *toComparableString(char buffer[MAXSTRLEN+1]) const {
        const signed char *ptr = &pieces[0][0];
        for (int i=0; i < 12; ++i) {
            for (int j=0; j < ptr[i]; ++j)
              *buffer++ = 'a'+i;
        }
        *buffer = '\0';
        return buffer;
    }
    bool scan(const char *text);
};

class GameState;

class StarSystem {
  public:
    std::string name;
    PieceCollection star;
    PieceCollection ships[NUMPLAYERS];
    int homeworldOf;

  public:
    /* Each system must have a valid name; a name is valid iff it is composed
     * of valid characters. Valid characters are alphanumeric plus some
     * punctuation, but whitespace is never allowed in system names. */
    static bool is_valid_name(const char *name);
    static bool is_valid_name_char(char ch);
    static const char *make_random_name(const GameState *st);

  public:
    StarSystem(): homeworldOf(-1) { }
    StarSystem(const char *n): name(n), homeworldOf(-1) { }
    int numberOfShips() const;
    bool hasNoShips() const;
    int numberOf(Color c) const;
    int number() const { return numberOfShips() + star.number(); }
    bool canCatastropheStar() const;
    bool canCatastrophe(Color c) const { return (this->numberOf(c) >= 4); }
    void performCatastrophe(Color c, PieceCollection &stash);
    bool playerHasAccessTo(int player, Color c) const
    { return (star.numberOf(c) > 0) || (ships[player].numberOf(c) > 0); }
    
    bool isAdjacentTo(const StarSystem &that) const {
        return this->star.isAdjacentTo(that.star);
    }
    PieceCollection pieceCollection() const {
        PieceCollection pc = star;
        for (int i=0; i < NUMPLAYERS; ++i)
          pc += ships[i];
        return pc;
    }
    
    std::string toString() const;
    std::string toComparableString() const;
    bool scan(const char *text);
};

PieceCollection& operator += (PieceCollection &, const StarSystem &);
PieceCollection& operator -= (PieceCollection &, const StarSystem &);

class GameState {
  public:
    std::vector<StarSystem> stars;
    PieceCollection stash;

  public:
    const StarSystem *systemNamed(const char *name) const;
    StarSystem *systemNamed(const char *name);
    const StarSystem *homeworldOf(int player) const;
    StarSystem *homeworldOf(int player);
    bool containsOverpopulation() const;

    void removeSystemNamed(const char *name);
    void removeSystem(StarSystem &star);
    void newGame();
    bool gameIsOver() const;

    /* Return a human-readable representation of this game state. */
    std::string toString() const;

    /* Return a compact representation of this game state that is guaranteed to be
     * identical to the compact representation of any equivalent game state, and
     * distinct from the compact representation of any non-equivalent game state.
     * In other words, two GameStates are "basically the same position" iff
     * g1.toComparableString() == g2.toComparableString(). */
    std::string toComparableString() const;

    /* Scan a human-readable representation of this game state, as produced
     * by toString(). Since the representation may span multiple lines, return
     * the first unparseable line as a string (so that we don't lose it). */
    std::string scan(FILE *fp);

    /* Flip the players: make player 0 player 1 and vice versa. */
    GameState mirror() const;
};

#endif /* H_STATE */

