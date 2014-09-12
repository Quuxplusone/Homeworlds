#pragma once

#include "state.h"
#include "move.h"

/* Given a move with missing pieces (e.g., "build g1" instead of
 * "build g1 at Earth"), infer the missing pieces from the state of
 * the game (for example, if the only place g1 could be built is at
 * Earth, then "build g1" must mean "build g1 at Earth").
 *   If the intended move is unambiguous, fill it in and return true.
 * If the move matches multiple non-equivalent possibilities, return
 * false (trashing the input move in the process).
 */
bool inferMoveFromState(const GameState &st, int attacker, WholeMove &move);
