
#include <vector>
#include "move.h"
#include "state.h"
#include "AllMoves.h"
#include "ApplyMove.h"
#include "GameStateEvaluator.h"
#include "PlanetNames.h"

WholeMove get_ai_move(const GameState &st, int attacker)
{
    static GameStateEvaluator evaluator("encoder_weights.txt", "evaluator_weights.txt");
    assert(!st.gameIsOver());

    std::vector<WholeMove> allmoves;
    findAllMoves_usualcase(st, attacker, allmoves);
    assert(!allmoves.empty());

    WholeMove *bestmove = &allmoves[0];
    GameState state_after_bestmove = st;
    ApplyMove::or_die(state_after_bestmove, attacker, *bestmove);

    for (WholeMove& move : allmoves) {
        GameState newst = st;
        ApplyMove::or_die(newst, attacker, move);
        if (evaluator.is_worse_than(state_after_bestmove, newst, attacker)) {
            bestmove = &move;
            state_after_bestmove = std::move(newst);
        }
    }
    reassignPlanetNames(*bestmove, st, nullptr);
    return std::move(*bestmove);
}
