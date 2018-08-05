
#include "state.h"
#include "move.h"
#include "ApplyMove.h"
#include "AI.h"

static int distanceBetween(const PieceCollection &a, const PieceCollection &b)
{
    if (a.isAdjacentTo(b)) return 1;
    if (a.numberOf(a.biggestSize()) == a.number()) return 2;
    if (b.numberOf(b.biggestSize()) == b.number()) return 2;
    if (a.numberOf(SMALL) == b.numberOf(SMALL) &&
        a.numberOf(MEDIUM) == b.numberOf(MEDIUM)) return 2;
    return 3;
}

/* Look for yellow sacrifices that cause a catastrophe in "c" at "here".
 * We break the attacker's ships up into tiers based on their distance
 * from "here". A catastrophe can be caused in any of the following ways:
 * (num_needed=3) Sacrifice y3; move three ships in from distance 1.
 * (num_needed=2) Sac y2 or y3; move two ships in from distance 1.
 * (num_needed=2) Sac y3; move one ship in from d=1 and one from d=2.
 * (num_needed=1) Sac y3; move one ship in from anywhere on the map.
 * (num_needed=1) Sac y2; move one ship in from d=1 or d=2.
 * (num_needed=1) Sac y1; move one ship in from d=1.
 * */
static int heuristic_catastrophe_penalty_yellow(const GameState &st,
                const StarSystem &here, int attacker, Color c,
                int base_cost, int worst_penalty_so_far)
{
    const int shipcost[3] = { 2, 4, 8 };
    const int num_needed = 4 - here.numberOf(c);
    assert(1 <= num_needed && num_needed <= 3);
    /* Figure out which of the above methods might apply. */
    int num_ships_at[4] = {0,0,0,0};
    int num_ships_ats[4][3] = {{0,0,0},{0,0,0},{0,0,0},{0,0,0}};
    int num_yellow_ships[3] = {0,0,0};

    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &star = st.stars[i];
        if (&star == &here) continue;
        const int d = distanceBetween(here.star, star.star);
        assert(1 <= d && d <= 3);
        num_ships_at[d] += star.ships[attacker].numberOf(c);
        num_ships_ats[d][SMALL] += star.ships[attacker].numberOf(c,SMALL);
        num_ships_ats[d][MEDIUM] += star.ships[attacker].numberOf(c,MEDIUM);
        num_ships_ats[d][LARGE] += star.ships[attacker].numberOf(c,LARGE);
        num_yellow_ships[SMALL] += star.ships[attacker].numberOf(YELLOW,SMALL);
        num_yellow_ships[MEDIUM] += star.ships[attacker].numberOf(YELLOW,MEDIUM);
        num_yellow_ships[LARGE] += star.ships[attacker].numberOf(YELLOW,LARGE);
    }
    if (num_needed == 3) {
        /* Sacrifice y3; move three ships in from distance 1 */
        if (num_yellow_ships[LARGE] == 0) return 0;
        if (num_ships_at[1] < 3) return 0;
        int smallmoved = std::min(3, num_ships_ats[1][SMALL]);
        int medmoved = std::min(3-smallmoved, num_ships_ats[1][MEDIUM]);
        int bigmoved = std::min(3-smallmoved-medmoved, num_ships_ats[1][LARGE]);
        assert(smallmoved + medmoved + bigmoved == 3);
        if (c == YELLOW) {
            assert(bigmoved <= num_yellow_ships[LARGE]);
            if (bigmoved == num_yellow_ships[LARGE]) return 0;
        }
        int penalty = base_cost + shipcost[LARGE]
                    + shipcost[LARGE]*bigmoved
                    + shipcost[MEDIUM]*medmoved
                    + shipcost[SMALL]*smallmoved;
        return std::min(penalty, worst_penalty_so_far);
    }
    if (num_needed == 2) {
        if (num_ships_at[1] >= 2 && (num_yellow_ships[LARGE] != 0 || num_yellow_ships[MEDIUM] != 0)) {
            /* Sacrifice y2 or y3; move two ships in from distance 1 */
            int smallmoved = std::min(2, num_ships_ats[1][SMALL]);
            int medmoved = std::min(2-smallmoved, num_ships_ats[1][MEDIUM]);
            int bigmoved = std::min(2-smallmoved-medmoved, num_ships_ats[1][LARGE]);
            assert(smallmoved + medmoved + bigmoved == 2);
            Size sacced = (num_yellow_ships[MEDIUM] > 0 ? MEDIUM : LARGE);
            if (c == YELLOW) {
                assert(bigmoved <= num_yellow_ships[LARGE]);
                assert(medmoved <= num_yellow_ships[MEDIUM]);
                if (medmoved == num_yellow_ships[MEDIUM]) {
                    sacced = LARGE;
                    if (bigmoved == num_yellow_ships[LARGE]) {
                        goto failed_alpha;
                    }
                }
            }
            int penalty = base_cost + shipcost[sacced]
                        + shipcost[LARGE]*bigmoved
                        + shipcost[MEDIUM]*medmoved
                        + shipcost[SMALL]*smallmoved;
            worst_penalty_so_far = std::min(penalty, worst_penalty_so_far);
        }
      failed_alpha:
        if (num_yellow_ships[LARGE] >= 1 &&
                 num_ships_at[2] >= 1 && num_ships_at[1] >= 1) {
            /* Sacrifice y3; move one ship in from d=1 and one from d=2. */
            int smallmoved1 = std::min(1, num_ships_ats[1][SMALL]);
            int medmoved1 = std::min(1-smallmoved1, num_ships_ats[1][MEDIUM]);
            int bigmoved1 = std::min(1-smallmoved1-medmoved1, num_ships_ats[1][LARGE]);
            int smallmoved2 = std::min(1, num_ships_ats[2][SMALL]);
            int medmoved2 = std::min(1-smallmoved2, num_ships_ats[2][MEDIUM]);
            int bigmoved2 = std::min(1-smallmoved2-medmoved2, num_ships_ats[2][LARGE]);
            assert(smallmoved1 + medmoved1 + bigmoved1 == 1);
            assert(smallmoved2 + medmoved2 + bigmoved2 == 1);
            const int smallmoved = smallmoved1 + smallmoved2;
            const int medmoved = medmoved1 + medmoved2;
            const int bigmoved = bigmoved1 + bigmoved2;
            if (c == YELLOW) {
                assert(bigmoved <= num_yellow_ships[LARGE]);
                if (bigmoved == num_yellow_ships[LARGE]) {
                    goto failed_beta;
                }
            }
            int penalty = base_cost + shipcost[LARGE]
                        + shipcost[LARGE]*bigmoved
                        + shipcost[MEDIUM]*medmoved
                        + shipcost[SMALL]*smallmoved;
            worst_penalty_so_far = std::min(penalty, worst_penalty_so_far);
        }
      failed_beta:
        return worst_penalty_so_far;
    }
    assert(num_needed == 1);
    if (num_ships_at[1] >= 1) {
        /* Move one ship in from d=1. */
        int smallmoved = std::min(1, num_ships_ats[1][SMALL]);
        int medmoved = std::min(1-smallmoved, num_ships_ats[1][MEDIUM]);
        int bigmoved = std::min(1-smallmoved-medmoved, num_ships_ats[1][LARGE]);
        assert(smallmoved + medmoved + bigmoved == 1);
        Size sacced = SMALL;
        if (num_yellow_ships[SMALL] == (c==YELLOW && smallmoved ? 1 : 0)) {
            sacced = MEDIUM;
            if (num_yellow_ships[MEDIUM] == (c==YELLOW && medmoved ? 1 : 0)) {
                sacced = LARGE;
                if (num_yellow_ships[LARGE] == (c==YELLOW && bigmoved ? 1 : 0)) {
                    goto failed_gamma;
                }
            }
        }
        int penalty = base_cost + shipcost[sacced]
                    + shipcost[LARGE]*bigmoved
                    + shipcost[MEDIUM]*medmoved
                    + shipcost[SMALL]*smallmoved;
        worst_penalty_so_far = std::min(penalty, worst_penalty_so_far);
    }
  failed_gamma:
    if (num_ships_at[2] >= 1) {
        /* Move one ship in from d=2. */
        int smallmoved = std::min(1, num_ships_ats[2][SMALL]);
        int medmoved = std::min(1-smallmoved, num_ships_ats[2][MEDIUM]);
        int bigmoved = std::min(1-smallmoved-medmoved, num_ships_ats[2][LARGE]);
        assert(smallmoved + medmoved + bigmoved == 1);
        Size sacced = MEDIUM;
        if (num_yellow_ships[MEDIUM] == (c==YELLOW && medmoved ? 1 : 0)) {
            sacced = LARGE;
            if (num_yellow_ships[LARGE] == (c==YELLOW && bigmoved ? 1 : 0)) {
                goto failed_epsilon;
            }
        }
        int penalty = base_cost + shipcost[sacced]
                    + shipcost[LARGE]*bigmoved
                    + shipcost[MEDIUM]*medmoved
                    + shipcost[SMALL]*smallmoved;
        worst_penalty_so_far = std::min(penalty, worst_penalty_so_far);
    }
  failed_epsilon:
    if (num_ships_at[3] >= 1 && num_yellow_ships[LARGE] >= 1) {
        /* Move one ship in from d=3. */
        int smallmoved = std::min(1, num_ships_ats[3][SMALL]);
        int medmoved = std::min(1-smallmoved, num_ships_ats[3][MEDIUM]);
        int bigmoved = std::min(1-smallmoved-medmoved, num_ships_ats[3][LARGE]);
        assert(smallmoved + medmoved + bigmoved == 1);
        if (c == YELLOW && num_yellow_ships[LARGE] == num_ships_ats[3][LARGE]) {
            goto failed_eta;
        }
        int penalty = base_cost + shipcost[LARGE]
                    + shipcost[LARGE]*bigmoved
                    + shipcost[MEDIUM]*medmoved
                    + shipcost[SMALL]*smallmoved;
        worst_penalty_so_far = std::min(penalty, worst_penalty_so_far);
    }
  failed_eta:
    return worst_penalty_so_far;
}


/* How convenient would it be for the defender to catastrophe color "c" at
 * this system? Look at how many ships the defender would have to lose,
 * versus how many ships the attacker would lose. Return a number that gets
 * more negative the more relative damage the defender can cause.
 * Return zero if the defender cannot catastrophe this color here, or if
 * the defender can catastrophe it only by giving up more of his own ships
 * than he causes the attacker to lose in the catastrophe.
 */
static int heuristic_catastrophe_penalty(const GameState &st,
                            const StarSystem &here, int attacker, Color c)
{
    const int shipcost[3] = { 2, 4, 8 };
    const int defender = 1-attacker;
    const int num_here = here.numberOf(c);
    if (num_here == 0) return 0;
    assert(1 <= num_here && num_here <= 3);
    /* Suppose all the pieces of color "c" here just vanished. What would that
     * do to the attacker's heuristic value? Things the defender would lose
     * add to the value; things the attacker would lose subtract from it. */
    int base_cost = 0;
    if (here.star.numberOf(c) == here.star.number()) {
        /* The defender is not allowed to destroy his own homeworld. */
        if (here.homeworldOf == defender) return 0;
        /* We needn't spend time on this if the defender already has a winning move. */
        if (here.homeworldOf == attacker) return 0;
        base_cost += shipcost[SMALL] * here.ships[defender].numberOf(SMALL);
        base_cost += shipcost[MEDIUM] * here.ships[defender].numberOf(MEDIUM);
        base_cost += shipcost[LARGE] * here.ships[defender].numberOf(LARGE);
        base_cost -= shipcost[SMALL] * here.ships[attacker].numberOf(SMALL);
        base_cost -= shipcost[MEDIUM] * here.ships[attacker].numberOf(MEDIUM);
        base_cost -= shipcost[LARGE] * here.ships[attacker].numberOf(LARGE);
    } else {
        base_cost += shipcost[SMALL] * here.ships[defender].numberOf(c,SMALL);
        base_cost += shipcost[MEDIUM] * here.ships[defender].numberOf(c,MEDIUM);
        base_cost += shipcost[LARGE] * here.ships[defender].numberOf(c,LARGE);
        base_cost -= shipcost[SMALL] * here.ships[attacker].numberOf(c,SMALL);
        base_cost -= shipcost[MEDIUM] * here.ships[attacker].numberOf(c,MEDIUM);
        base_cost -= shipcost[LARGE] * here.ships[attacker].numberOf(c,LARGE);
        /* Add a penalty for catastrophing part of a homeworld. */
        if (here.star.numberOf(c) != 0) {
            base_cost += 20;
        }
    }

    int worst_penalty_so_far = 0;
    /* Can we catastrophe this color here with a free move? */
    if (num_here == 3) {
        if (here.ships[defender].numberOf(c) != 0 &&
                st.stash.numberOf(c) != 0 &&
                here.playerHasAccessTo(defender, GREEN)) {
            /* The defender can just build a new ship here. */
            const int penalty = base_cost;
            if (penalty < worst_penalty_so_far) {
                worst_penalty_so_far = penalty;
            }
        } else {
            for (int i=0; i < (int)st.stars.size(); ++i) {
                const StarSystem &star = st.stars[i];
                if (!star.isAdjacentTo(here)) continue;
                if (!star.playerHasAccessTo(defender, YELLOW)) continue;
                if (star.ships[defender].numberOf(c) == 0) continue;
                if (star.homeworldOf == defender && star.ships[defender].number() == 1) {
                    continue;
                }
                /* The defender could move a ship from "star" to "here" for free,
                 * thus catastrophing "c" at "here". How much would it hurt him?
                 * He'd lose the ship he moved, plus the base cost. */
                const int penalty = base_cost + shipcost[star.ships[defender].smallestSizeOf(c)];
                if (penalty < worst_penalty_so_far) {
                    worst_penalty_so_far = penalty;
                }
            }
        }
    }
    /* Can we catastrophe this color with a sacrifice move? */
    const int num_needed = 4 - num_here;
    assert(1 <= num_needed && num_needed <= 3);
    if (here.ships[defender].numberOf(c) != 0 &&
            st.stash.numberOf(c) + (c==GREEN) >= num_needed) {
        /* If we can find a big enough green to sacrifice, we can do it.
         * Notice that if (c==GREEN), it never pays to sacrifice green at
         * the site of the catastrophe; there must be at least two of our
         * green there in order to build green back, plus at least one of
         * the opponent's green pieces, which means we could just have built
         * the fourth green for free instead of sacrificing. */
        for (Size s = (Size)(num_needed-1); s <= LARGE; ++s) {
            for (int i=0; i < (int)st.stars.size(); ++i) {
                const StarSystem &star = st.stars[i];
                if (&star == &here && c == GREEN) continue;
                if (star.ships[defender].numberOf(GREEN,s) == 0) continue;
                if (star.homeworldOf == defender && star.ships[defender].number() == 1) {
                    continue;
                }
                const int penalty = base_cost + shipcost[s];
                if (penalty < worst_penalty_so_far) {
                    worst_penalty_so_far = penalty;
                }
                goto done_green;
            }
        }
      done_green: ;
    }
    if (here.ships[defender].number() - here.ships[defender].numberOf(c) >= num_needed &&
            st.stash.numberOf(c) + (c==BLUE) >= num_needed) {
        /* If we can find a big enough blue to sacrifice, we might be able
         * to do it. We don't bother to check whether the sizes in the stash
         * actually match up with the sizes of the defender's ships;
         * TODO FIXME BUG HACK. (Honestly, it's highly unusual for someone to
         * cause a catastrophe with a blue sacrifice in the first place.) */
        for (Size s = (Size)(num_needed-1); s <= LARGE; ++s) {
            for (int i=0; i < (int)st.stars.size(); ++i) {
                const StarSystem &star = st.stars[i];
                if (star.ships[defender].numberOf(BLUE,s) == 0) continue;
                if (star.homeworldOf == defender && star.ships[defender].number() == 1) {
                    continue;
                }
                if (&star == &here && c == BLUE && here.ships[defender].number() == num_needed) {
                    continue;
                }
                const int penalty = base_cost + shipcost[s];
                if (penalty < worst_penalty_so_far) {
                    worst_penalty_so_far = penalty;
                }
                goto done_blue;
            }
        }
      done_blue: ;
    }
    /* Finally, the case that we're usually worried about: the big yellow
     * sacrifice (a.k.a. the "Bluebird Mistake"). */
    worst_penalty_so_far = heuristic_catastrophe_penalty_yellow(
        st, here, defender, c, base_cost, worst_penalty_so_far
    );
    /* Done! */
    assert(worst_penalty_so_far <= 0);
    return worst_penalty_so_far;
}


/* Return an estimated value of the given position after the given move.
 * Return a high value if the attacker is winning, and a low value if
 * he's losing. When considering the stash, note that it will be the
 * defender's turn next. */
int ai_static_evaluation(const GameState &state_before_move,
                         const WholeMove &move, int attacker)
{
    GameState st_ = state_before_move;
    ApplyMove::or_die(st_, attacker, move);
    const GameState &st = st_;

    const int defender = 1-attacker;
    const StarSystem *hw[2] = { nullptr, nullptr };
    bool can_build[2][4] = {};
    bool has_factory[2] = {};
    bool has_gun[2] = {};
    int sum = 0;

    /* Check for places to safely build green; i.e., places with only one green ship. */
    for (int player=0; player <= 1; ++player) {
        for (int i=0; i < (int)st.stars.size(); ++i) {
            const StarSystem &star = st.stars[i];
            if (!has_gun[player] && star.ships[player].numberOf(RED) > 0) {
                has_gun[player] = true;
            }
            if (!can_build[player][GREEN] && !star.star.numberOf(GREEN)) {
                if (star.ships[player].numberOf(GREEN) == 1) {
                    can_build[player][GREEN] = true;
                }
            }
            if (star.homeworldOf == player) {
                hw[player] = &star;
            }
        }
    }
    assert(hw[0] != nullptr);
    assert(hw[1] != nullptr);

    /* Check for factories. */
    bool medium_factory_possible = (st.stash.numberOf(GREEN,SMALL) == 0);
    bool large_factory_possible = (medium_factory_possible && st.stash.numberOf(GREEN,MEDIUM) == 0);
    bool large_factory_possible_with_greenbuild =
            (st.stash.numberOf(GREEN,SMALL) + st.stash.numberOf(GREEN,MEDIUM) == 1);
    if (medium_factory_possible || large_factory_possible || large_factory_possible_with_greenbuild) {
        for (int player=0; player <= 1; ++player) {
            for (int i=0; i < (int)st.stars.size(); ++i) {
                const StarSystem &star = st.stars[i];
                /* A factory at a green star would be dangerous anyway. */
                if (star.star.numberOf(GREEN)) continue;
                if (star.ships[player].numberOf(GREEN) != 2) continue;
                if (star.ships[player].numberOf(GREEN,MEDIUM) && medium_factory_possible) {
                    has_factory[player] = true;
                    break;
                } else if (star.ships[player].numberOf(GREEN,LARGE)) {
                    if (large_factory_possible) {
                        has_factory[player] = true;
                        break;
                    } else if (large_factory_possible_with_greenbuild && can_build[player][GREEN]) {
                        has_factory[player] = true;
                        break;
                    }
                }
            }
        }
    }

    /* We can "safely" build a particular color if we have a factory and
     * somewhere safe to put it, or somewhere safe that also contains green.
     */
    for (int player=0; player <= 1; ++player) {
        for (Color c = RED; c <= BLUE; ++c) {
            for (int i=0; i < (int)st.stars.size(); ++i) {
                const StarSystem &star = st.stars[i];
                /* Can "player" safely build color "c" at "star"? */
                int n = star.ships[player].numberOf(c);
                if (n == 0 || n == 3) continue;
                if (n == 2) continue;
                if (star.playerHasAccessTo(player, GREEN) || has_factory[player]) {
                    can_build[player][c] = true;
                    break;
                }
            }
        }
    }


    /* Stash economics. For each color...
     *   If only the defender can build it safely, that's a point for him.
     *   If only the attacker can build it, that's a point for him.
     *   If we both can build it, but there are two pieces at the bottom
     * of the stash or only one piece remaining in the stash, that's a point
     * for the defender. */
    for (Color c = RED; c <= BLUE; ++c) {
        if (st.stash.numberOf(c) == 0) continue;
        Size s = st.stash.smallestSizeOf(c);
        if (can_build[defender][c]) {
            if (can_build[attacker][c]) {
                if (st.stash.numberOf(c,s) == 2) {
                    sum -= (s == SMALL ? 0 : s == MEDIUM ? 40 : 80);
                } else if (st.stash.number() == 1) {
                    sum -= (s == SMALL ? 10 : s == MEDIUM ? 40 : 80);
                }
            } else {
                sum -= (s == SMALL ? 0 : s == MEDIUM ? 50 : 100);
            }
        } else {
            if (can_build[attacker][c]) {
                sum += (s == SMALL ? 10 : s == MEDIUM ? 40 : 80);
            } else {
                /* Nobody can build it safely. */
            }
        }
    }

    /* Points for material advantage... */
    PieceCollection all_ships[2];
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &star = st.stars[i];
        all_ships[0] += star.ships[0];
        all_ships[1] += star.ships[1];
    }
    int absolute_value_all_ships[2];
    for (int player=0; player <= 1; ++player) {
        absolute_value_all_ships[player] =  20*all_ships[player].numberOf(SMALL);
        absolute_value_all_ships[player] += 50*all_ships[player].numberOf(MEDIUM);
        absolute_value_all_ships[player] += 110*all_ships[player].numberOf(LARGE);
    }
    sum += (absolute_value_all_ships[attacker] - absolute_value_all_ships[defender]);

    if (all_ships[attacker].number() > 4 && all_ships[attacker].numberOf(YELLOW) == 0) {
        sum -= 40;
    }
    if (all_ships[defender].number() > 4 && all_ships[defender].numberOf(YELLOW) == 0) {
        sum += 40;
    }
    if (all_ships[attacker].number() > 4 && all_ships[attacker].numberOf(GREEN) == 0) {
        sum -= 30;
    }
    if (all_ships[defender].number() > 4 && all_ships[defender].numberOf(GREEN) == 0) {
        sum += 30;
    }

    if (has_gun[attacker] && all_ships[attacker].numberOf(LARGE) >= 2) {
        sum += 100;
    }
    if (has_gun[defender] && all_ships[defender].numberOf(LARGE) >= 2) {
        sum -= 100;
    }

    /* Look specifically at the two homeworlds now. */
    for (int player=0; player <= 1; ++player) {
        const StarSystem &home = *hw[player];
        const int m = (player == attacker ? 1 : -1);
        if (home.star.number() == 1) {
            /* Penalty for having part of the homeworld catastrophed. */
            sum -= m*300;
        }
        if (home.ships[player].numberOf(LARGE) == 0) {
            /* Penalty for having no defense at the homeworld. */
            sum -= m*100;
            if (home.star.numberOf(RED) != 0) {
                sum -= m*30;
            }
        }
        if (has_gun[1-player] && !home.playerHasAccessTo(player, RED)) sum -= m*30;
        if (home.playerHasAccessTo(player, YELLOW)) sum += m*50;
        if (home.playerHasAccessTo(player, GREEN)) sum += m*60;
        if (home.playerHasAccessTo(player, BLUE)) sum += m*70;
    }

    /* Now look at each star individually. */
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &star = st.stars[i];
        const bool in_contention = !star.ships[attacker].empty() && !star.ships[defender].empty();
        bool attacker_advantage = false;
        if (in_contention) {
            const Size ab = star.ships[attacker].biggestSize();
            const Size db = star.ships[defender].biggestSize();
            attacker_advantage = (ab > db);
            if (attacker_advantage && all_ships[attacker].numberOf(RED) >= 2) {
                sum += 30;
            } else if (!attacker_advantage && has_gun[defender]) {
                sum -= (ab == LARGE ? 100 : 50);
            }
        }

        /* Positive points for diversity. */
        int diversity[2] = {0,0};
        const int diversity_lookup_table[8] = {0, 10, 10, 30, 50, 110, 150, 220 };
        for (int player=0; player <= 1; ++player) {
            if (star.ships[player].empty()) {
                diversity[player] = 0;
            } else {
                diversity[player] += 1 * star.playerHasAccessTo(player, RED);
                diversity[player] += 2 * star.playerHasAccessTo(player, YELLOW);
                diversity[player] += 2 * star.playerHasAccessTo(player, GREEN);
                diversity[player] += 2 * star.playerHasAccessTo(player, BLUE);
            }
            assert(0 <= diversity[player] && diversity[player] <= 7);
        }
        if (in_contention && !attacker_advantage) {
            /* don't count this diversity of potential capture-victims */
            sum -= diversity[attacker]/2;
        } else {
            sum += diversity_lookup_table[diversity[attacker]];
        }
        if (in_contention && attacker_advantage) {
            /* don't count this diversity of potential capture-victims */
            sum += diversity[defender]/2;
        } else {
            sum -= diversity_lookup_table[diversity[defender]];
        }

        /* If there's only one ship here, and no way to build more, then that's
         * a slight penalty. */
        if (star.ships[attacker].number() == 1 &&
                !has_factory[attacker] && !star.playerHasAccessTo(attacker,GREEN)) {
            sum -= 20;
        }
        if (star.ships[defender].number() == 1 &&
                !has_factory[defender] && !star.playerHasAccessTo(defender,GREEN)) {
            sum += 20;
        }
        /* If this is a blue star, then we'd like to have some ships available
         * to convert. */
        if (star.star.numberOf(BLUE) != 0 && !in_contention &&
                star.ships[attacker].number() >= 2) {
            sum += 20;
        }
        if (star.star.numberOf(BLUE) != 0 && !in_contention &&
                star.ships[defender].number() >= 2) {
            sum += 20;
        }

        /* Penalty for creating systems containing only green and red. */
        if (star.ships[attacker].number() != 0 &&
            !star.playerHasAccessTo(attacker,BLUE) &&
            !star.playerHasAccessTo(attacker,YELLOW) &&
            !(in_contention && attacker_advantage)) {
            sum -= 40;
        }

        /* Mild penalty for creating systems in no-man's-land. */
        if (star.ships[attacker].number() != 0 &&
            !star.isAdjacentTo(*hw[0]) && !star.isAdjacentTo(*hw[1])) {
            sum -= 10;
        }

        /* Mild penalty for creating "green ship, yellow star" systems:
         * systems where all my ships are green and the star is yellow,
         * red, or (extremely unlikely) also green. This may be unavoidable
         * in the early game, or when the stash is low. */
        if (star.ships[attacker].number() == star.ships[attacker].numberOf(GREEN) &&
            star.star.numberOf(BLUE) == 0) {
            sum -= 10;
        }

        /* Negative points for leaving enough of one color in a system that
         * the defender can produce a beneficial catastrophe on his turn. */
        if (!star.ships[attacker].empty()) {
            for (Color c = RED; c <= BLUE; ++c) {
                sum += heuristic_catastrophe_penalty(st, star, attacker, c);
            }
        }

        /* Being able to move a big ship into an undefended homeworld is a
         * big plus. */
        if (all_ships[attacker].numberOf(YELLOW) != 0 &&
                hw[defender]->ships[defender].numberOf(LARGE) == 0 &&
                star.isAdjacentTo(*hw[defender]) &&
                star.ships[attacker].numberOf(LARGE) != 0) {
            sum += (has_gun[attacker] ? 70 : 40);
        }
        if (all_ships[defender].numberOf(YELLOW) != 0 &&
                hw[attacker]->ships[attacker].numberOf(LARGE) == 0 &&
                star.isAdjacentTo(*hw[attacker]) &&
                star.ships[defender].numberOf(LARGE) != 0) {
            sum -= (has_gun[defender] ? 80 : 60);
        }
    }

    return sum;
}
