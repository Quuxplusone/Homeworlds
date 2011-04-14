
#ifndef H_AI
 #define H_AI

#include <vector>
#include "state.h"
#include "move.h"

/* Given a state and a move, return a high value if the move looks good
 * and a low value if it looks bad. */
int ai_static_evaluation(const GameState &state_before_move,
                         const WholeMove &move, int attacker);

void get_all_moves_sorted_by_value(const GameState &st,
        int attacker, std::vector<WholeMove> &retmoves, bool rate_moves);

WholeMove get_ai_move(const GameState &st, int attacker);

#endif /* H_AI */
