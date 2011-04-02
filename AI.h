
#ifndef H_AI
 #define H_AI

#include <vector>
#include "state.h"
#include "move.h"

/* Given a state, return a high value if it looks good for player_who_moved,
 * and a low value if it looks bad for player_who_moved. Notice that it is
 * basically never true that evaluation(S,0) == -evaluation(S,1), mostly
 * because our heuristic isn't very good, but also because a good stash
 * position boosts the value of the state to the "current player", no matter
 * which player's turn it is. */
int ai_static_evaluation(const GameState &state_after_move, int player_who_moved);

void get_all_moves_sorted_by_value(const GameState &st,
        int attacker, std::vector<WholeMove> &retmoves, bool rate_moves);

WholeMove get_ai_move(const GameState &st, int attacker);



#endif /* H_AI */
