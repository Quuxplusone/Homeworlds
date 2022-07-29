
/* Read in a state, then print the number of successor states
 * (that is, "number of moves" excluding duplicates),
 * including "pass" as a valid move.
 */

#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <string>
#include "state.h"
#include "AllMoves.h"
#include "PlanetNames.h"

static void do_error(const char *msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    printf("Error: ");
    vprintf(msg, ap);
    putchar('\n');
    va_end(ap);
    exit(EXIT_FAILURE);
}

void print_counts(const GameState& st, int attacker, const std::string& playername)
{
    int count = 0;
    int countPass = 0;
    int countSac[4] = {};
    int countFree[4] = {};
    (void)findAllMoves(
        st, attacker,
        false, false, 0xf,
        [&](const WholeMove& m, const GameState&) {
            ++count;
            const auto *action = m.firstNonCatastropheAction();
            if (action == nullptr) {
                countPass += 1;
            } else if (action->kind == SACRIFICE) {
                countSac[action->piece.color] += 1;
            } else if (action->kind == CAPTURE) {
                countFree[RED] += 1;
            } else if (action->kind == MOVE || action->kind == MOVE_CREATE) {
                countFree[YELLOW] += 1;
            } else if (action->kind == BUILD) {
                countFree[GREEN] += 1;
            } else if (action->kind == CONVERT) {
                countFree[BLUE] += 1;
            } else {
                assert(false);
            }
            return false;
        }
    );

    printf("%s has %d unique moves:\n", playername.c_str(), count);
    printf("- %d sac r, %d free r\n", countSac[RED], countFree[RED]);
    printf("- %d sac y, %d free y\n", countSac[YELLOW], countFree[YELLOW]);
    printf("- %d sac g, %d free g\n", countSac[GREEN], countFree[GREEN]);
    printf("- %d sac b, %d free b\n", countSac[BLUE], countFree[BLUE]);
    printf("- %d pass\n", countPass);
}

int main()
{
    GameState st;
    std::string firstline = st.scan(stdin);
    assert(firstline == "");
    StarSystem *hw0 = st.homeworldOf(0);
    StarSystem *hw1 = st.homeworldOf(1);
    if (hw0 == nullptr) {
        do_error("The initial homeworld setup didn't include Player 0's homeworld!");
    } else if (hw1 == nullptr) {
        do_error("The initial homeworld setup didn't include Player 1's homeworld!");
    }
    std::string player0 = hw0->name.empty() ? "Player 0" : hw0->name;
    std::string player1 = hw1->name.empty() ? "Player 1" : hw1->name;
    assignPlanetNames(&st);
    printf("The stash is: %s\n", st.stash.toString().c_str());
    print_counts(st, 0, player0);
    print_counts(st, 1, player1);
}
