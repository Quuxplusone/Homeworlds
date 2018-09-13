
#include <assert.h>
#include <string>
#include "state.h"
#include "move.h"
#include "ApplyMove.h"


/* Perform the given "move", updating "st" to reflect the new game state.
 * If "move" is valid, then return success (and update "st" accordingly).
 * If "move" is invalid, then return the reason, and leave garbage in "st".
 *
 * Notice that "move" may involve blowing up the attacker's homeworld with
 * a catastrophe; this is done intentionally, to simplify
 * combine_postcatastrophes() in "AllMoves.cc".
 *
 * Note that ApplyMove is a class rather than a namespace so that it can
 * be a "friend" of classes WholeMove and SingleAction.
 */
ApplyMove::Result ApplyMove::Single(GameState &st, int attacker, const SingleAction &action)
{
    StarSystem *where = st.systemNamed(action.where.c_str());
    if (where == nullptr) return Result::UNKNOWN_NAME;
    const Color c = action.color;
    const Size s = action.size;

    switch (action.kind) {
        case CATASTROPHE: {
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (!where->containsOverpopulation(c)) return Result::IMPOSSIBLE;
            where->performCatastrophe(c, st.stash);
            /* If this star has been destroyed, remove its entry. */
            if (where->star.empty()) {
                /* performCatastrophe() should have removed all the
                 * pieces already. */
                assert(where->number() == 0);
                st.removeSystem(*where);
                where = nullptr;
            }
            break;
        }
        case SACRIFICE: {
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (s == UNKNOWN_SIZE) return Result::AMBIGUOUS;
            if (where->ships[attacker].numberOf(c,s) == 0) return Result::IMPOSSIBLE;
            where->ships[attacker].remove(c,s);
            st.stash.insert(c,s);
            break;
        }
        case CAPTURE: {
            /* Assume the player has access to RED here, or has sacrificed. */
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (s == UNKNOWN_SIZE) return Result::AMBIGUOUS;
            if (where->ships[attacker].numberAtLeast(s) == 0) return Result::IMPOSSIBLE;
            const int defender = (1-attacker);
            if (where->ships[defender].numberOf(c,s) == 0) return Result::IMPOSSIBLE;
            where->ships[defender].remove(c,s);
            where->ships[attacker].insert(c,s);
            break;
        }
        case MOVE: {
            /* Assume the player has access to YELLOW here, or has sacrificed. */
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (s == UNKNOWN_SIZE) return Result::AMBIGUOUS;
            if (where->ships[attacker].numberOf(c,s) == 0) return Result::IMPOSSIBLE;
            StarSystem *dest = st.systemNamed(action.whither.c_str());
            if (dest == nullptr) return Result::UNKNOWN_NAME;
            if (!dest->isAdjacentTo(*where)) return Result::IMPOSSIBLE;
            where->ships[attacker].remove(c,s);
            dest->ships[attacker].insert(c,s);
            break;
        }
        case MOVE_CREATE: {
            /* Assume the player has access to YELLOW here, or has sacrificed. */
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (s == UNKNOWN_SIZE) return Result::AMBIGUOUS;
            if (where->ships[attacker].numberOf(c,s) == 0) return Result::IMPOSSIBLE;
            StarSystem *dest = st.systemNamed(action.whither.c_str());
            if (dest != nullptr) return Result::DUPLICATE_NAME;
            /* Create a new star system. */
            if (st.stash.numberOf(action.newcolor, action.newsize) == 0) return Result::IMPOSSIBLE;
            /* Unfortunately, push_back() invalidates pointers into the vector,
             * so we have to save and recalculate "where". */
            const int wherei = (where - &st.stars[0]);
            st.stars.push_back(StarSystem());
            where = &st.stars[wherei];
            /* All right, now continue. */
            dest = &st.stars.back();
            dest->name = action.whither;
            st.stash.remove(action.newcolor, action.newsize);
            dest->star.insert(action.newcolor, action.newsize);
            if (!dest->isAdjacentTo(*where)) return Result::IMPOSSIBLE;
            where->ships[attacker].remove(c,s);
            dest->ships[attacker].insert(c,s);
            break;
        }
        case BUILD: {
            /* Assume the player has access to GREEN here, or has sacrificed. */
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (s == UNKNOWN_SIZE) return Result::AMBIGUOUS;
            if (st.stash.numberOf(c,s) == 0) return Result::IMPOSSIBLE;
            if (st.stash.smallestSizeOf(c) != s) return Result::IMPOSSIBLE;
            if (where->ships[attacker].numberOf(c) == 0) return Result::IMPOSSIBLE;
            st.stash.remove(c,s);
            where->ships[attacker].insert(c,s);
            break;
        }
        case CONVERT: {
            /* Assume the player has access to BLUE here, or has sacrificed. */
            if (c == UNKNOWN_COLOR) return Result::AMBIGUOUS;
            if (s == UNKNOWN_SIZE) return Result::AMBIGUOUS;
            if (st.stash.numberOf(action.newcolor,s) == 0) return Result::IMPOSSIBLE;
            if (where->ships[attacker].numberOf(c,s) == 0) return Result::IMPOSSIBLE;
            where->ships[attacker].remove(c,s);
            st.stash.insert(c,s);
            st.stash.remove(action.newcolor,s);
            where->ships[attacker].insert(action.newcolor,s);
            break;
        }
        default: assert(false);
    }
    /* If an action has left this system with no ships in it,
     * then return the star to the stash. */
    if (where != nullptr && where->hasNoShips()) {
        if (where->homeworldOf == attacker) return Result::SUICIDE;
        st.stash += where->star;
        st.removeSystem(*where);
    }
    /* Technically, we might want to return false if this is an action
     * that would lose the game for the attacker (a SACRIFICE, CATASTROPHE,
     * MOVE, or MOVE_CREATE that leaves his homeworld undefended). But
     * that check would be very expensive, especially since this routine
     * is called from findAllMoves() --- so we'll omit it. */
    return Result::SUCCESS;
}

ApplyMove::Result ApplyMove::Whole(GameState &st, int attacker, const WholeMove &move)
{
    assert(move.sanitycheck());
    assert(st.homeworldOf(attacker) != nullptr);
    assert(st.homeworldOf(1-attacker) != nullptr);
    bool saw_sacrifice = false;
    for (int i=0; i < (int)move.actions.size(); ++i) {
        const SingleAction &action = move.actions[i];
        switch (action.kind) {
            case SACRIFICE: {
                saw_sacrifice = true;
                break;
            }
            case CAPTURE:
            case MOVE:
            case MOVE_CREATE:
            case BUILD:
            case CONVERT: {
                Color color = UNKNOWN_COLOR;
                bool UNUSED(success) = action.getAssociatedColor(&color);
                assert(success);
                StarSystem *where = st.systemNamed(action.where.c_str());
                if (where == nullptr) return Result::UNKNOWN_NAME;
                if (!saw_sacrifice && !where->playerHasAccessTo(attacker, color)) return Result::IMPOSSIBLE;
                break;
            }
            default:
                break;
        }
        auto action_result = ApplyMove::Single(st, attacker, action);
        if (action_result != Result::SUCCESS) return action_result;
    }
    /* A losing move is not a legal move. */
    if (st.hasLost(attacker)) {
        return Result::SUICIDE;
    }
    assert(st.homeworldOf(attacker) != nullptr);
    if (st.hasLost(1-attacker) && st.homeworldOf(attacker)->containsOverpopulation()) {
        GameState newst = st;
        newst.performAllCatastrophes();
        if (newst.hasLost(attacker)) {
            return Result::SUICIDE;
        }
    }
    /* Otherwise, we succeeded in making the whole move. */
    return Result::SUCCESS;
}

/* Return true if the given "move" is a valid move for "player", starting from
 * the game state "st". Otherwise, return false.
 */
bool ApplyMove::isValidMove(const GameState &st, int attacker, const WholeMove &move)
{
    GameState newstate = st;
    Result result = ApplyMove::Whole(newstate, attacker, move);
    return result == Result::SUCCESS;
}

/* Perform the given "move", updating "st" to reflect the new game state.
 * If "move" is not a valid move, assert failure.
 */
void ApplyMove::or_die(GameState &st, int attacker, const WholeMove &move)
{
    Result UNUSED(result) = ApplyMove::Whole(st, attacker, move);
    assert(result == Result::SUCCESS);
}

/* Perform the given "action", updating "st" to reflect the new game state.
 * If "action" is not a valid move, assert failure.
 */
void ApplyMove::or_die(GameState &st, int attacker, const SingleAction &action)
{
    Result UNUSED(result) = ApplyMove::Single(st, attacker, action);
    assert(result == Result::SUCCESS);
}
