#pragma once

#include "state.h"
#include "WholeMove.h"
#include <vector>

/* Given a state, return a high value if the state looks good
 * and a low value if it looks bad (to the player who just moved). */
int ai_static_evaluation(const GameState &state_after_move,
        int who_just_moved);

std::vector<WholeMove> get_all_moves_sorted_by_value(const GameState &st,
        int attacker, bool rate_moves);

WholeMove get_ai_move(const GameState &st, int attacker);
