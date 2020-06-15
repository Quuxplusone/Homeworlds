#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include "state.h"
#include "AllMoves.h"
#include "WholeMove.h"

WholeMove defenders_best_response_or_pass(const GameState& st, bool deep);

bool defender_is_in_check(const GameState& st)
{
    return findWinningMove(st, 0, nullptr);
}

bool attacker_can_still_checkmate(const GameState& st)
{
    return findAllMoves(
        st, 0, false, false, 0xF,
        [&](const WholeMove&, const GameState& newst) {
            if (defender_is_in_check(newst)) {
                WholeMove response = defenders_best_response_or_pass(newst, false);
                return response.isPass();
            }
            return false;
        }
    );
}

WholeMove defenders_best_response_or_pass(const GameState& st, bool deep)
{
    WholeMove response("pass");
    findAllMoves(
        st, 1, false, false, 0xF,
        [&response, deep](const WholeMove& m, const GameState& newst) {
            if (newst.gameIsOver()) {
                // The defender has won!
                response = m;
                return true;
            } else if (!defender_is_in_check(newst)) {
                // The defender has successfully staved off checkmate.
                if (deep && attacker_can_still_checkmate(newst)) {
                    // Never mind, we've still got a checkmate in 2.
                } else {
                    response = m;
                    return true;
                }
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
    bool deep = (argc >= 2 && argv[1] == std::string("--deep"));

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
                WholeMove response = defenders_best_response_or_pass(newst, deep);
                if (response.isPass()) {
                    moves_creating_checkmate.push_back(m);
                } else {
                    moves_creating_check.emplace_back(m, response);
                }
            }
            return false;
        }
    );
    printf("%zu moves lead to checkmate%s\n", moves_creating_checkmate.size(), deep ? " in two" : "");
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
        bool found[4] {};
        for (auto&& mr : moves_creating_check) {
            int cr = (mr.first.toString().find("capture r") != size_t(-1));
            int cy = (mr.first.toString().find("capture y") != size_t(-1));
            int cg = (mr.first.toString().find("capture g") != size_t(-1));
            int cb = (mr.first.toString().find("capture b") != size_t(-1));
            if (cr + cy + cg + cb == 1) {
                found[cr*0 + cy*1 + cg*2 + cb*3] = true;
            }
        }
        printf("Found: %c%c%c%c\n", found[0] ? 'R':' ', found[1]?'Y':' ', found[2]?'G':' ', found[3]?'B':' ');
        for (Color c : {RED, YELLOW, GREEN, BLUE, UNKNOWN_COLOR}) {
            auto yes = [c](const auto& mr) {
                return associatedColor(mr.first) == c;
            };
            int count = std::count_if(moves_creating_check.begin(), moves_creating_check.end(), yes);
            printf("- %d of %s\n", count, Piece(c, SMALL).toString());
        }
    }
}
