
#include <assert.h>
#include <stdio.h>
#include <algorithm>
#include <vector>
#include "state.h"
#include "move.h"
#include "AI.h"
#include "PlanetNames.h"
#include "ApplyMove.h"
#include "AllMoves.h"

/* Return true if this move would put the current attacker in check, thus
 * causing him to lose the game on the very next turn. The AI should never
 * make such a move. */
static bool move_is_stupid_move_into_check(const GameState &st, int attacker, const WholeMove &move)
{
    GameState newst = st;
    ApplyMove::or_die(newst, attacker, move);
    /* A winning move is definitely not a stupid move into check. */
    if (newst.gameIsOver()) {
        return false;
    }
    /* At this point, we basically want to find all winning moves for the
     * defender, and return true iff we found a winning move. But searching
     * for moves --- even just the winning ones --- is slow, so we keep
     * the most recently seen "killer moves" cached here. If this state is
     * susceptible to one of these killer moves, then we don't need to
     * look for other ways for the defender to win. */
    static WholeMove killer[8];
    static int killeridx = 0;
    for (int i=0; i < 8; ++i) {
        GameState newst2 = newst;
        if (ApplyMove::Whole(newst2, 1-attacker, killer[i])) {
            if (newst2.gameIsOver()) {
                return true;
            }
        }
    }
    /* None of the killer moves were useful in this scenario.
     * Search for any winning move. */
    WholeMove winmove;
    if (!findWinningMove(newst, 1-attacker, &winmove)) {
        return false;
    }
    /* Found a new killer move! */
    killer[killeridx] = winmove;
    killeridx = (killeridx+1) % 8;
    return true;
}


struct val_move_pair {
    WholeMove *move;
    int value;
    /* Sort high values toward the front of the array. */
    bool operator < (const val_move_pair &rhs) const {
        return (this->value > rhs.value);
    }
};


/* If "rate_moves" is TRUE, then dump the 10 top-rated moves to stdout
 * so the user can see why we're choosing a particular move.
 */
void get_all_moves_sorted_by_value(const GameState &st,
        int attacker, std::vector<WholeMove> &retmoves, bool rate_moves)
{
    assert(retmoves.empty());

    std::vector<WholeMove> allmoves;
    findAllMoves_usualcase(st, attacker, allmoves);
    assert(!allmoves.empty());
    const int n = allmoves.size();
    if (n == 1) {
        retmoves.push_back(allmoves[0]);
        return;
    }

    /* Assign each new state a value according to our heuristic. */
    std::vector<val_move_pair> values(n);
    values.resize(n);
    for (int i=0; i < n; ++i) {
        values[i].move = &allmoves[i];
        values[i].value = ai_static_evaluation(st, allmoves[i], attacker);
    }
    std::sort(values.begin(), values.end());
    /* Now copy the moves into the vector to return. */
    assert(retmoves.empty());
    retmoves.resize(n);
    if (rate_moves) {
        const StarSystem *attacker_hw = st.homeworldOf(attacker);
        assert(attacker_hw != NULL);
        printf("%s has %d possible moves.\n", attacker_hw->name.c_str(), n);
        for (int i=0; i < n && i < 10; ++i) {
            printf("value=%d: %s\n", values[i].value,
                    values[i].move->toString().c_str());
        }
    }
    for (int i=0; i < n; ++i) {
        retmoves[i] = *values[i].move;
    }
    return;
}

WholeMove get_ai_move(const GameState &st, int attacker)
{
    assert(!st.gameIsOver());
    std::vector<WholeMove> allmoves;
    get_all_moves_sorted_by_value(st, attacker, allmoves, false);
    const int n = allmoves.size();
    for (int i=0; i < n; ++i) {
        WholeMove &bestmove = allmoves[i];
        if (move_is_stupid_move_into_check(st, attacker, bestmove)) {
            continue;
        }
        /* This move is okay. */
        reassignPlanetNames(bestmove, st, NULL);
        return bestmove;
    }
    /* All possible moves led into check! */
    return WholeMove("pass");
}
