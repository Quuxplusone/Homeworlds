#pragma once

#include "move.h"
#include "state.h"

/* Given the game state "st", append all possible moves for player "attacker"
 * to the given vector "allmoves". There are usually going to be a few dozen
 * to a few hundred moves, but it might be 500,000 for really complicated
 * game states.
 *   If the flag "prune_obviously_worse_moves" if true, then we simplify
 * matters by cutting short our search as soon as a winning move is found
 * (if one is found). However, we do guarantee that we'll always report the
 * best move; we'll never try to estimate a move's "goodness" based on a
 * heuristic. However, even if "prune_obviously_worse_moves" is false, we
 * will not generate multiple *equivalent* moves; e.g., the move list will
 * not include both "sac b1 at Beta; convert y1 to b1 at Beta" and
 * "sac y1 at Beta", because those moves are completely equivalent.
 *   If the flag "look_only_for_wins" is true, then look only for immediately
 * winning moves; don't report any move that's not an immediate win. This may
 * result in an empty vector. If "look_only_for_wins" is false, we'll always
 * return at least the move "pass".
 *   The bitmask "these_colors_only" holds a value such as
 * ((1 << YELLOW) | (1 << BLUE)), which means "look only for yellow and blue
 * moves". This includes sacrifice moves as well as free moves, and includes
 * moves involving catastrophes as well.
 */
void findAllMoves(const GameState &st, int attacker,
    std::vector<WholeMove> &allmoves,
    bool prune_obviously_worse_moves,
    bool look_only_for_wins,
    unsigned int these_colors_only);

/* Usually, we want prune_obviously_worse_moves=true,
 * look_only_for_wins=false, and these_colors_only=0xF. */
void findAllMoves_usualcase(const GameState &st, int attacker,
    std::vector<WholeMove> &allmoves);

/* Sometimes, we just want to know a single bit of information:
 * Does the attacker have a winning move from this position, or not?
 * If "move" is not NULL, then we fill it in with the winning move. */
bool findWinningMove(const GameState &st, int attacker, WholeMove *move);
