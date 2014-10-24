
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include "state.h"


const char *StarSystem::make_random_name(const GameState *st)
{
    static char buffer[7] = "UuXXXX";
    static unsigned int counter = 0;
    do {
        buffer[2] = 'a' + (counter & 0xF);
        buffer[3] = 'a' + ((counter >> 4) & 0xF);
        buffer[4] = 'a' + ((counter >> 8) & 0xF);
        buffer[5] = 'a' + ((counter >> 12) & 0xF);
        assert(buffer[6] == '\0');
        ++counter;
        /* If there's already a star with this name, just increment the
         * counter and try again. There certainly can't be 65536 stars
         * in the galaxy; there aren't that many pieces in the stash!
         */
    } while (st != NULL && st->systemNamed(buffer) != NULL);
    assert(StarSystem::is_valid_name(buffer));
    return buffer;
}


bool StarSystem::is_valid_name_char(char ch)
{
    return isalnum(ch) || ch == '_' || ch == '\'' || ch == '/';
}

bool StarSystem::is_valid_name(const char *name)
{
    for (int i=0; name[i] != '\0'; ++i) {
        if (!StarSystem::is_valid_name_char(name[i]))
          return false;
    }
    return true;
}

std::string StarSystem::toString() const
{
    std::string result = name;
    if (name != "")
      result += ' ';
    result += '(';
    if (homeworldOf >= 0) {
        result += mprintf("%d, ", homeworldOf);
    }
    result += star.toString();
    result += ") ";
    result += ships[0].toString();
    for (int i=1; i < NUMPLAYERS; ++i) {
        result += '-';
        result += ships[i].toString();
    }
    return result;
}

/* Scan a star system, in the format
 *   "Earth (0, g1b3) r1y2b1y3-b2b3"
 * or
 *   "Mars (r2) -r3r3"
 * or
 *   "(b1) g1-"
 * The hyphen between Player 0's and Player 1's ships is required.
 * If no name is given for this star, we'll just use the empty string.
 * We assume that leading and trailing whitespace has already been trimmed,
 * and that the string contains the representation of just one star system.
 */
bool StarSystem::scan(const char *text)
{
    std::string unwhited_text(text);
    int textlen = unwhited_text.length();
    int j = 0;
    for (int i=0; i < textlen; ++i) {
        if (!isspace(text[i]))
          unwhited_text[j++] = text[i];
    }
    unwhited_text.resize(j);
    text = unwhited_text.c_str();
    /* We now have a copy of "text" with all the whitespace removed. */
    name = "";
    star.clear();
    for (int i=0; i < NUMPLAYERS; ++i)
      ships[i].clear();

    const char *paren = strchr(text, '(');
    if (paren == NULL)
      return false;
    if (paren == text) {
        name = "";
    } else {
        /* Get the name of this star system. */
        int namelength = paren - text;
        assert(text[namelength] == '(');
        assert(namelength > 0);
        name.assign(text, namelength);
        if (!is_valid_name(name.c_str()))
          return false;
    }
    /* Look at the character following '('. If it's a digit, we're scanning
     * some player's homeworld. Otherwise, we're scanning a non-homeworld
     * (and this character had better be one of 'r', 'y', 'g', 'b'). */
    text = paren+1;
    if (isdigit(*text)) {
        assert(NUMPLAYERS <= 9);
        this->homeworldOf = (*text - '0');
        if (this->homeworldOf >= NUMPLAYERS) return false;
        ++text;
        if (*text != ',') return false;
        ++text;
    } else {
        this->homeworldOf = -1;
    }

    /* Everything between the current "text" pointer and the following ')'
     * must be the pieces that make up the star itself. */
    paren = strchr(text, ')');
    if (paren == NULL) return false;
    std::string star_pieces(text, paren-text);
    if (!star.scan(star_pieces.c_str()))
      return false;

    /* After the ')' we get into the pieces representing the players' ships.
     * The ship lists are separated by '-'. */
    text = paren+1;
    for (int i=0; i < NUMPLAYERS; ++i) {
        const char sentinel = (i == NUMPLAYERS-1) ? '\0' : '-';
        const char *dash = strchr(text, sentinel);
        if (dash == NULL) return false;
        /* Note that PieceCollection::scan("") returns false, so if
         * the piecelist is empty, we just skip the call to scan(). */
        if (dash != text) {
            std::string ship_pieces(text, dash-text);
            if (!ships[i].scan(ship_pieces.c_str()))
              return false;
        }
        if (i != NUMPLAYERS-1)
          text = dash+1;
    }
    return true;
}

int StarSystem::numberOfShips() const
{
    int count = 0;
    for (int i=0; i < NUMPLAYERS; ++i)
      count += ships[i].number();
    return count;
}

bool StarSystem::hasNoShips() const
{
    for (int i=0; i < NUMPLAYERS; ++i)
      if (!ships[i].empty()) return false;
    return true;
}

int StarSystem::numberOf(Color c) const
{
    int count = star.numberOf(c);
    for (int i=0; i < NUMPLAYERS; ++i)
      count += ships[i].numberOf(c);
    return count;
}

bool StarSystem::canCatastropheStar() const
{
    for (Color c = RED; c <= BLUE; ++c) {
        if (star.numberOf(c) == 0) continue;
        if (!this->containsOverpopulation(c))
          return false;
    }
    return true;
}

bool StarSystem::containsOverpopulation() const
{
    return this->containsOverpopulation(RED) ||
           this->containsOverpopulation(YELLOW) ||
           this->containsOverpopulation(GREEN) ||
           this->containsOverpopulation(BLUE);
}

/* Remove all pieces of color "c" from this system, and return them to
 * the designated "stash". If this causes the system to be depopulated,
 * or catastrophes the star, then return all the pieces to the stash.
 * Note that the star system doesn't know about the overall game state,
 * so it won't automatically remove itself from the vector "st.stars". */
void StarSystem::performCatastrophe(Color c, PieceCollection &stash)
{
    /* Cull the star's pieces of color "c". */
    for (Size s = SMALL; s <= LARGE; ++s) {
        stash.insert(c, s, star.numberOf(c,s));
    }
    star.removeAll(c);
    if (star.number() == 0) {
        /* If the star has been destroyed, then destroy all the ships here. */
        for (int i=0; i < NUMPLAYERS; ++i) {
            stash += ships[i];
            ships[i].clear();
        }
        return;
    }
    /* Cull the ships of color "c". */
    for (Size s = SMALL; s <= LARGE; ++s) {
        int count = 0;
        for (int i=0; i < NUMPLAYERS; ++i)
          count += ships[i].numberOf(c,s);
        stash.insert(c, s, count);
    }
    for (int i=0; i < NUMPLAYERS; ++i)
      ships[i].removeAll(c);
    if (numberOfShips() == 0) {
        /* If the last ship has been destroyed, then destroy the star. */
        stash += star;
        star.clear();
    }
    return;
}

PieceCollection& operator += (PieceCollection &lhs, const StarSystem &rhs)
{
    lhs += rhs.star;
    for (int i=0; i < NUMPLAYERS; ++i)
      lhs += rhs.ships[i];
    return lhs;
}

PieceCollection& operator -= (PieceCollection &lhs, const StarSystem &rhs)
{
    lhs -= rhs.star;
    for (int i=0; i < NUMPLAYERS; ++i)
      lhs -= rhs.ships[i];
    return lhs;
}
