
#include <assert.h>
#include <stdio.h>
#include <algorithm>
#include <functional>
#include <vector>
#include "state.h"
#include "AI.h"
#include "AllMoves.h"
#include "ApplyMove.h"
#include "PlanetNames.h"
#include "WholeMove.h"

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
        if (ApplyMove::Whole(newst2, 1-attacker, killer[i]) == ApplyMove::Result::SUCCESS) {
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
    killer[killeridx] = std::move(winmove);
    killeridx = (killeridx+1) % 8;
    return true;
}

/* If "rate_moves" is TRUE, then dump the 10 top-rated moves to stdout
 * so the user can see why we're choosing a particular move.
 */
std::vector<WholeMove> get_all_moves_sorted_by_value(const GameState &st,
        int attacker, bool rate_moves)
{
    std::vector<WholeMove> allmoves;
    std::vector<std::pair<int, int>> values;

    findAllMoves(
        st, attacker,
        /*prune_obviously_worse_moves=*/true,
        /*look_only_for_wins=*/false,
        0xF,
        [&](const WholeMove& move, const GameState& state_after_move) {
            // Assign each new state a value according to our heuristic.
            allmoves.emplace_back(move);
            values.emplace_back(
                ai_static_evaluation(state_after_move, attacker),
                allmoves.size() - 1
            );
            return false;
        }
    );
    const int n = values.size();

    std::sort(values.begin(), values.end(), std::greater<>());
    if (rate_moves) {
        const StarSystem *attacker_hw = st.homeworldOf(attacker);
        assert(attacker_hw != nullptr);
        printf("%s has %d possible moves.\n", attacker_hw->name.c_str(), n);
        for (int i=0; i < n && i < 100; ++i) {
            printf("value=%d: %s\n", values[i].first,
                    allmoves[values[i].second].toString().c_str());
        }
    }
    /* Now transfer the moves into the vector to return. */
    std::vector<WholeMove> retmoves;
    retmoves.reserve(n);
    for (auto&& elt : values) {
        retmoves.push_back(std::move(allmoves[elt.second]));
    }
    return retmoves;
}

WholeMove get_ai_move(const GameState &st, int attacker)
{
    assert(!st.gameIsOver());
    std::vector<WholeMove> allmoves = get_all_moves_sorted_by_value(st, attacker, false);
    for (WholeMove& bestmove : allmoves) {
        if (move_is_stupid_move_into_check(st, attacker, bestmove)) {
            continue;
        }
        /* This move is okay. */
        reassignPlanetNames(&bestmove, st);
        return std::move(bestmove);
    }
    /* All possible moves led into check! */
    return WholeMove("pass");
}
