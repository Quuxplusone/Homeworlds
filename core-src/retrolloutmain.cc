#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include "state.h"
#include "AllMoves.h"
#include "Retrograde.h"
#include "WholeMove.h"

bool defender_is_in_check(const GameState& st)
{
    return findWinningMove(st, 0, nullptr);
}

bool contains_one_blue_ship(const GameState& st)
{
    int sum = 0;
    for (const auto& ss : st.stars) {
        sum += ss.ships[0].numberOf(BLUE);
    }
    return (sum == 1);
}

bool moves_only_the_blue_ship(const WholeMove& m)
{
    bool found = false;
    for (const auto& a : m.actions) {
        switch (a.kind) {
            case MOVE: case MOVE_CREATE:
                if (a.piece.color != BLUE) return false;
                found = true;
                break;
            case SACRIFICE: case CATASTROPHE:
                break;
            default:
                return false;
        }
    }
    return found;
}

int main(int argc, char **argv)
{
    GameState st;
    st.scan(stdin);

    // "If you had moved your blue ship INSTEAD..."
    findAllRetrograde(
        st, 0, false, false, 0xF,
        [&](const WholeMove& m, const GameState& prevst) {
            if (!contains_one_blue_ship(prevst)) return false;
            if (moves_only_the_blue_ship(m)) return false;
            bool success = findAllMoves(
                prevst, 0, false, false, (1u << YELLOW),
                [&](const WholeMove& m, const GameState& hypst) {
                    if (!moves_only_the_blue_ship(m)) return false;
                    if (!defender_is_in_check(hypst)) return false;
                    // "Ah, but then you would have catastrophed half of my homeworld."
                    bool success = findAllMoves(
                        hypst, 1, false, false, (1u << YELLOW) | (1u << GREEN) | (1u << BLUE),
                        [&](const WholeMove& m, const GameState& st3) {
                            const StarSystem *lee = st3.homeworldOf(0);
                            if (lee && lee->star.number() == 1) {
                                return true;
                            }
                            return false;
                        }
                    );
                    return success;
                }
            );
            if (success) {
                printf("%s\n", prevst.toString().c_str());
                printf("Lee's actual move => %s\n", m.toString().c_str());
            }
            return false;
        }
    );
}
