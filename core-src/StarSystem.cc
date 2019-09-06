
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include "mprintf.h"
#include "state.h"

bool StarSystem::isValidName(const char *name)
{
    auto is_valid_name_char = [](char ch) {
        return isalnum(ch) || ch == '_';
    };
    for (int i=0; name[i] != '\0'; ++i) {
        if (!is_valid_name_char(name[i])) {
            return false;
        }
    }
    return true;
}

std::string StarSystem::toString() const
{
    std::string result = name;
    if (name != "") {
        result += ' ';
    }
    result += '(';
    if (homeworldOf >= 0) {
        result += mprintf("%d, ", homeworldOf);
    }
    result += star.toString();
    result += ") ";
    bool first = true;
    for (const auto& fleet : ships) {
        if (!std::exchange(first, false)) {
            result += '-';
        }
        result += fleet.toString();
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
        if (!isspace(text[i])) {
            unwhited_text[j++] = text[i];
        }
    }
    unwhited_text.resize(j);
    text = unwhited_text.c_str();
    /* We now have a copy of "text" with all the whitespace removed. */
    name = "";
    star.clear();
    for (auto& fleet : ships) {
        fleet.clear();
    }

    const char *paren = strchr(text, '(');
    if (paren == nullptr) {
        return false;
    } else if (paren == text) {
        name = "";
    } else {
        /* Get the name of this star system. */
        int namelength = paren - text;
        assert(text[namelength] == '(');
        assert(namelength > 0);
        name.assign(text, namelength);
        if (!isValidName(name.c_str())) {
            return false;
        }
    }
    /* Look at the character following '('. If it's a digit, we're scanning
     * some player's homeworld. Otherwise, we're scanning a non-homeworld
     * (and this character had better be one of 'r', 'y', 'g', 'b'). */
    text = paren+1;
    if (isdigit(*text)) {
        static_assert(2 <= NUMPLAYERS && NUMPLAYERS <= 9, "");
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
    if (paren == nullptr) return false;
    std::string star_pieces(text, paren-text);
    if (!star.scan(star_pieces.c_str())) {
        return false;
    }

    /* After the ')' we get into the pieces representing the players' ships.
     * The ship lists are separated by '-'. */
    text = paren+1;
    for (int i=0; i < NUMPLAYERS; ++i) {
        const char sentinel = (i == NUMPLAYERS-1) ? '\0' : '-';
        const char *dash = strchr(text, sentinel);
        if (dash == nullptr) return false;
        /* Note that PieceCollection::scan("") returns false, so if
         * the piecelist is empty, we just skip the call to scan(). */
        if (dash != text) {
            std::string ship_pieces(text, dash-text);
            if (!ships[i].scan(ship_pieces.c_str())) {
                return false;
            }
        }
        if (i != NUMPLAYERS-1) {
            text = dash+1;
        }
    }
    return true;
}

int StarSystem::numberOfShips() const
{
    int count = 0;
    for (const auto& fleet : ships) {
        count += fleet.number();
    }
    return count;
}

bool StarSystem::hasNoShips() const
{
    for (const auto& fleet : ships) {
        if (!fleet.empty()) {
            return false;
        }
    }
    return true;
}

int StarSystem::numberOf(Color c) const
{
    int count = star.numberOf(c);
    for (const auto& fleet : ships) {
        count += fleet.numberOf(c);
    }
    return count;
}

bool StarSystem::canCatastropheStar() const
{
    for (Color c = RED; c <= BLUE; ++c) {
        if (star.numberOf(c) == 0) continue;
        if (!this->containsOverpopulation(c)) {
            return false;
        }
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
        for (auto& fleet : ships) {
            stash += fleet;
            fleet.clear();
        }
        return;
    }

    /* Cull the ships of color "c". */
    int count1 = 0;
    int count2 = 0;
    int count3 = 0;
    for (auto& fleet : ships) {
        count1 += fleet.numberOf(c, SMALL);
        count2 += fleet.numberOf(c, MEDIUM);
        count3 += fleet.numberOf(c, LARGE);
        fleet.removeAll(c);
    }
    stash.insert(c, SMALL, count1);
    stash.insert(c, MEDIUM, count2);
    stash.insert(c, LARGE, count3);

    if (numberOfShips() == 0) {
        /* If the last ship has been destroyed, then destroy the star. */
        stash += star;
        star.clear();
    }
}
