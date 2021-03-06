#pragma once

#include "SingleAction.h"
#include "WholeMove.h"
#include "state.h"

/* Note that ApplyMove is a class rather than a namespace so that it can
 * be a "friend" of classes WholeMove and SingleAction. */
struct ApplyMove {

    enum Result {
        SUCCESS,
        UNKNOWN_NAME,
        DUPLICATE_NAME,
        AMBIGUOUS,
        IMPOSSIBLE,
        SUICIDE,
    };

    /* If the given "action" is legal to perform from this state, perform it
     * and return success, updating "st" in place. If the given action is invalid,
     * then return the reason, leaving "st" in an inconsistent state.
     *   Note that even a losing move (such as catastrophing your own homeworld)
     * is considered legal in this context.
     */
    static Result Single(GameState &st, int attacker, const SingleAction &action);

    /* If the given "move" is legal to perform from this state, perform it
     * and return success, updating "st" in place. If any part of the given move
     * is invalid, then return the reason, leaving "st" in an inconsistent state.
     *   Note that even a losing move (such as catastrophing your own homeworld)
     * is considered legal in this context.
     */
    static Result Whole(GameState &st, int attacker, const WholeMove &move);

    /* Perform the given "move", updating "st" to reflect the new game state.
     * If "move" is not a valid move, assert failure.
     *   Note that even a losing move (such as catastrophing your own homeworld)
     * is considered legal in this context.
     */
    static void or_die(GameState &st, int attacker, const SingleAction &action);
    static void or_die(GameState &st, int attacker, const WholeMove &move);

    /* Return true if the given "move" is a valid move for "player", starting from
     * the game state "st". Otherwise, return false.
     */
    static bool isValidMove(const GameState &st, int attacker, const WholeMove &move);
};
