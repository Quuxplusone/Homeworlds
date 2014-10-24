
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <algorithm>
#include <string>
#include <vector>
#include "state.h"


const StarSystem *GameState::systemNamed(const char *name) const
{
    return const_cast<GameState*>(this)->systemNamed(name);
}

StarSystem *GameState::systemNamed(const char *name)
{
    for (int i=0; i < (int)stars.size(); ++i) {
        if (stars[i].name == name)
          return &stars[i];
    }
    return NULL;
}

const StarSystem *GameState::homeworldOf(int player) const
{
    return const_cast<GameState*>(this)->homeworldOf(player);
}

StarSystem *GameState::homeworldOf(int player)
{
    assert(0 <= player && player < NUMPLAYERS);
    for (int i=0; i < (int)stars.size(); ++i) {
        if (stars[i].homeworldOf == player)
          return &stars[i];
    }
    return NULL;
}

/* Returns true if there is an overpopulation of any color anywhere in
 * the galaxy. The significance of this is that there should never be any
 * overpopulations remaining at the end of a turn, if both players are
 * playing optimally; this is an invariant on which our AI relies. */
bool GameState::containsOverpopulation() const
{
    for (int i=0; i < (int)stars.size(); ++i) {
        if (stars[i].containsOverpopulation())
          return true;
    }
    return false;
}

void GameState::performAllCatastrophes()
{
    for (int i=0; i < (int)stars.size(); ++i) {
        bool did_one = false;
        for (Color c = RED; c <= BLUE; ++c) {
            if (stars[i].containsOverpopulation(c)) {
                stars[i].performCatastrophe(c, stash);
                did_one = true;
            }
        }
        if (did_one && stars[i].number() == 0) {
            stars.erase(stars.begin()+i);
            --i;
        }
    }
}


void GameState::removeSystemNamed(const char *name)
{
    for (int i=0; i < (int)stars.size(); ++i) {
        if (stars[i].name == name) {
            stars.erase(stars.begin()+i);
            return;
        }
    }
    assert(false);
}

void GameState::removeSystem(StarSystem &star)
{
    size_t i = (&star - &stars[0]);
    assert(i < stars.size());
    stars.erase(stars.begin()+i);
}


/* Set up for a two-player game. */
void GameState::newGame()
{
    stars.clear();
    stash.clear();
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            stash.insert(c, s, 3);
        }
    }
}

bool GameState::hasLost(int who) const
{
    const StarSystem *hw = homeworldOf(who);
    return (hw == NULL || hw->ships[who].empty());
}

std::string GameState::toString() const
{
    std::string result = "";
    for (int i=0; i < (int)stars.size(); ++i) {
        result += stars[i].toString();
        result += '\n';
    }
    return result;
}

/* Read a complete line from the given file and return it in "line".
 * Return true if anything was read; otherwise, return false. */
static bool readline(FILE *fp, std::string &line)
{
    line = "";
    for (int k = getc(fp); k != EOF; k = getc(fp)) {
        if (k == '\n') {
            return true;
        } else if (k == '#') {
            while (k != EOF && k != '\n')
              k = getc(fp);
            return true;
        }
        line += (char)k;
    }
    /* The file ended without a newline, or it was
     * already at end-of-file. Return TRUE if we
     * read anything from this last line. */
    return (line.length() > 0);
}

/* Trim whitespace from both ends of the given string. */
static void trim(std::string &line)
{
    int start, end;
    for (start = 0; start < (int)line.length() && isspace(line[start]); ++start)
      /* continue */ ;
    for (end = line.length()-1; end >= 0 && isspace(line[end]); --end)
      /* continue */ ;
    line.erase(end+1);
    line.erase(0, start);
}


/* Read a sequence of lines from the given file, where each line matches
 * the representation expected by StarSystem::scan()... except that blank
 * lines are ignored, and lines ending with "# comment text" are trimmed
 * before processing.
 *   Because our input is line-buffered, we read a bunch of matching lines
 * and then we read one non-matching line before returning. We don't want
 * to just throw away that line, so we return it as a std::string.
 *   If we can parse everything in the file, then we return "". This is
 * not ambiguous, because we consider a blank line (or a line consisting
 * only of a comment) to be parseable, and thus we will never return ""
 * in any other circumstance.
 */
std::string GameState::scan(FILE *fp)
{
    std::string unparsed_line = "";
    stars.clear();
    while (true) {
        std::string line;
        bool gotline = readline(fp, line);
        if (!gotline)
          break;
        trim(line);
        if (line.length() == 0)
          continue;
        StarSystem star;
        bool parsedline = star.scan(line.c_str());
        if (!parsedline) {
            unparsed_line = line;
            break;
        }
        stars.push_back(star);
    }
    stash.clear();
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            stash.insert(c, s, NUMPLAYERS+1);
        }
    }
    for (int i=0; i < (int)stars.size(); ++i) {
        stash -= stars[i];
    }
    return unparsed_line;
}

GameState GameState::mirror() const
{
    assert(NUMPLAYERS == 2);
    GameState ret = *this;
    for (int i=0; i < (int)ret.stars.size(); ++i) {
        StarSystem star = ret.stars[i];
        PieceCollection t = star.ships[0];
        star.ships[0] = star.ships[1];
        star.ships[1] = t;
        if (star.homeworldOf != -1)
          star.homeworldOf = 1-star.homeworldOf;
    }
    return ret;
}
