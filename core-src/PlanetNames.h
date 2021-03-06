#pragma once

#include "state.h"
#include "WholeMove.h"

/* The given move may contain SingleActions of kind MOVE_CREATE, where
 * the names given to the new systems are nonsense names. For each new
 * system name mentioned in this move, rename it to one of the names
 * from "names" which "st" does not yet use. Note that in a two-player
 * game, at most 18 systems can exist at once and at most 3 systems can
 * be created during a single turn, so at most 21 names are needed.
 *   This function is provided for two reasons. One: findAllMoves()
 * cycles through names of the form "Uuaaaa" pretty quickly, without
 * checking whether the name already exists, so this is needed for
 * correctness when playing a long game against the AI. Two: The human
 * player will find human-readable names easier to keep track of than
 * internal "Uuaaaa" names.
 * This function does not need to be called in programs that just
 * look one move ahead and then quit.
 */
void reassignPlanetNames(WholeMove *move, const GameState& st);
void reassignPlanetNames(WholeMove *move, const GameState& st, const char *names[21]);

/* The given state may contain stars with no name. Assign a unique
 * name to each unnamed star system.
 */
void assignPlanetNames(GameState *st);
void assignPlanetNames(GameState *st, const char *names[21]);

/* The given move is legal in state "st", and "st" is isomorphic to
 * "st2", but possibly with different system names. Update "move" in place
 * so that it is legal relative to state "st2" instead.
 */
void reassignNamesToMove(WholeMove *move, const GameState& st, const GameState& st2);
