#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include "state.h"
#include "AllMoves.h"
#include "WholeMove.h"

bool defender_is_in_check(const GameState& st)
{
    return findWinningMove(st, 0, nullptr);
}

WholeMove defenders_best_response_or_pass(const GameState& st)
{
    WholeMove response("pass");
    findAllMoves(
        st, 1, false, false, 0xF,
        [&response](const WholeMove& m, const GameState& newst) {
            if (!defender_is_in_check(newst)) {
                response = m;
                return true;
            }
            return false;
        }
    );
    return response;
}

Color associatedColor(const WholeMove& m)
{
    Color c = UNKNOWN_COLOR;
    for (const SingleAction& a : m.getActions()) {
        if (a.getAssociatedColor(&c)) break;
    }
    return c;
}

int main(int argc, char **argv)
{
    bool verbose = (argc >= 2 && argv[1] == std::string("--verbose"));

    GameState st;
    st.scan(stdin);
    WholeMove move;
    if (findWinningMove(st, 0, &move)) {
        printf("oops, here's an immediately winning move!\n+ %s\n", move.toString().c_str());
        abort();
    }
    puts("Here we go...\n\n");
    std::vector<std::pair<WholeMove, WholeMove>> moves_creating_check;
    std::vector<WholeMove> moves_creating_checkmate;
    findAllMoves(
        st, 0, false, false, 0xF,
        [&](const WholeMove& m, const GameState& newst) {
            if (defender_is_in_check(newst)) {
                WholeMove response = defenders_best_response_or_pass(newst);
                if (response.isPass()) {
                    moves_creating_checkmate.push_back(m);
                } else {
                    moves_creating_check.emplace_back(m, response);
                }
            }
            return false;
        }
    );
    printf("%zu moves lead to checkmate\n", moves_creating_checkmate.size());
    for (const auto& m : moves_creating_checkmate) {
        printf("+ %s\n", m.toString().c_str());
    }
    printf("%zu moves lead to check but not checkmate\n", moves_creating_check.size());
    if (verbose) {
        for (const auto& mr : moves_creating_check) {
            printf("- %s\n", mr.first.toString().c_str());
            printf("  - %s\n", mr.second.toString().c_str());
        }
    } else {
        for (Color c : {RED, YELLOW, GREEN, BLUE, UNKNOWN_COLOR}) {
            auto yes = [c](const auto& mr) {
                return associatedColor(mr.first) == c;
            };
            int count = std::count_if(moves_creating_check.begin(), moves_creating_check.end(), yes);
            printf("- %d of %s\n", count, Piece(c, SMALL).toString());
        }
    }
}
