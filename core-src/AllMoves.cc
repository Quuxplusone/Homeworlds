
#include <assert.h>
#include <string.h>
#include <string>
#include <vector>
#include "state.h"
#include "move.h"
#include "ApplyMove.h"
#include "AllMoves.h"

/* When append_move() finds a winning move, it throws an exception all the
 * way back to findAllMoves() in order to shortcut the rest of the search.
 * If you don't want to rely on C++ exceptions, set this flag to zero; this
 * will turn on checks of all.done at various points in the search, so
 * that the shortcutting behavior is not entirely lost. This flag does not
 * affect the observable behavior of findAllMoves(); the same list of moves
 * is returned in each case. */
#ifndef ALLMOVES_USE_EXCEPTIONS
 #define ALLMOVES_USE_EXCEPTIONS 1
#endif

/* GNU's hash_map<> container is much faster than std::map<> for our purposes.
 * This flag does not affect the observable behavior of findAllMoves(), but
 * it will speed up the map operations.
 */
#if !defined(ALLMOVES_USE_UNORDERED_MAP) && !defined(ALLMOVES_USE_GNU_HASH_MAP)
 #define ALLMOVES_USE_UNORDERED_MAP 1
#endif

struct PossCat {
    Color color;
    std::string name;
    PossCat(Color c, const std::string &n): color(c), name(n) { }
};


#if ALLMOVES_USE_UNORDERED_SET
#include <unordered_set>
using AllSeenT = std::unordered_set<std::string>;
#else
#include <set>
using AllSeenT = std::set<std::string>;
#endif

struct AllT {
    std::string original_state;
    AllSeenT seen;
    std::function<bool(const WholeMove&, const GameState&)> callback;
    const bool prune_worse_moves;
    const bool win_only;
    const bool contained_overpopulations;
    bool look_for[4];
    bool done = false;

    explicit AllT(
        std::function<bool(const WholeMove&, const GameState&)> cb,
        bool p, bool w, bool co, unsigned int mask
    ) :
        callback(std::move(cb)),
        prune_worse_moves(p), win_only(w),
        contained_overpopulations(co)
    {
        for (Color c = RED; c <= BLUE; ++c) {
            look_for[c] = ((mask & (1u << (int)c)) != 0);
        }
    }

    bool emplace_into_seen(std::string key) {
        auto pair = seen.equal_range(key);
        if (pair.first == pair.second) {
            seen.emplace_hint(pair.first, std::move(key));
            return true;
        }
        return false;
    }

    void be_done() {
        done = true;
#if ALLMOVES_USE_EXCEPTIONS
        throw 42;
#endif
    }
};


static void combine_precatastrophes(const WholeMove &m, const std::vector<PossCat> &posscats, int pc,
    const GameState &st, int attacker, AllT &all);
static void combine_normal(const WholeMove &m, const GameState &st, int attacker, AllT &all);
static void combine_n_actions(const WholeMove &m, Color color, int num_moves,
    const GameState &st, int attacker, AllT &all);
static void combine_one_action(const WholeMove &m, Color color, int num_more_moves,
    int staridx, const GameState &st, int attacker, AllT &all);
static void combine_one_red_action(const WholeMove &m, int num_more_moves,
    int staridx, const GameState &st, int attacker, AllT &all);
static void combine_one_yellow_action(const WholeMove &m, int num_more_moves,
    int staridx, const GameState &st, int attacker, AllT &all);
static void combine_one_green_action(const WholeMove &m, int num_more_moves,
    int staridx, const GameState &st, int attacker, AllT &all);
static void combine_one_blue_action(const WholeMove &m, int num_more_moves,
    int staridx, const GameState &st, int attacker, AllT &all);
static void finish_combine_one_action(const WholeMove &newm, Color color,
    int num_more_moves, const GameState &newst, int attacker, AllT &all);
static void find_postcatastrophes(const WholeMove &m,
    const GameState &st, int attacker, AllT &all);
static void combine_postcatastrophes(const WholeMove &m,
    const std::vector<PossCat> &posscats, int pc, const GameState &st,
    int attacker, AllT &all);
static void append_move(AllT &all, const WholeMove &m, const GameState &st, int attacker);


std::vector<WholeMove> findAllMoves(const GameState &st, int attacker,
    bool prune_worse_moves,
    bool look_only_for_wins,
    unsigned int these_colors_only)
{
    std::vector<WholeMove> allmoves;
    findAllMoves(
        st, attacker,
        prune_worse_moves, look_only_for_wins, these_colors_only,
        [&](const WholeMove& m, const GameState&) {
            allmoves.push_back(m);
            return false;
        }
    );
    return allmoves;
}

bool findWinningMove(const GameState &st, int attacker, WholeMove *move)
{
    std::vector<WholeMove> winning_moves = findAllMoves(
        st, attacker,
        /*prune_obviously_worse_moves=*/true,
        /*look_only_for_wins=*/true,
        0xF
    );
    if (!winning_moves.empty()) {
        if (move != nullptr) {
            *move = std::move(winning_moves[0]);
        }
        return true;
    }
    return false;
}

/* The short description: Given the game state "st", report all possible moves
 * for player "attacker" via the "callback". We'll find all the possible
 * moves by recursive descent. If "win_only" is true, then don't bother
 * reporting any move that's not an immediate win for "attacker".
 *   Now the real-world caveats: We expect this function to be used for
 * alpha-beta search, not as an actual list of *all* possible moves from a
 * given state. Therefore, if we can rigorously prove that move A is strictly
 * worse than move B, we won't bother to return it.
 *   Example #1: If we are looking only for winning moves, well, one winning
 * move is as good as any other, so we needn't bother to add all of them.
 *   Example #2: If we've already seen a winning move, we needn't search any
 * further --- even if we were asked for "all moves".
 *   Example #3: A move of "pass" (or even a move of "catastrophe ...; pass")
 * never leads to a win assuming perfect play by both sides (because if it
 * did, then the defender could simply "pass" in response, leading to a draw;
 * contradiction). So we won't bother to report a possible move of "pass"
 * unless it is the *only* available move.
 */
bool findAllMoves(const GameState &st, int attacker,
    bool prune_worse_moves,
    bool look_only_for_wins,
    unsigned int these_colors_only,
    const std::function<bool(const WholeMove&, const GameState&)>& callback)
{
    /* Precondition: We assume the game isn't over; if it's over, then the
     * caller shouldn't have called findAllMoves() in the first place. */
    assert(!st.gameIsOver());

    AllT all(callback, prune_worse_moves, look_only_for_wins,
             st.containsOverpopulation(), these_colors_only);
    /* As explained above, we want to avoid generating moves that are
     * equivalent to "pass". It's easy to avoid generating "pass", but
     * harder to avoid "sacrifice g1 at Where; build g1 at Where", which
     * is equivalent to "pass". So we save the old state and have
     * append_move() reject moves whose new state looks like the old
     * state. */
    all.original_state = st.toComparableString();

#if ALLMOVES_USE_EXCEPTIONS
    try {
#endif

    WholeMove passmove;
    if (all.contained_overpopulations) {
        /* Top level: Find all the pre-catastrophes we can make. */
        std::vector<PossCat> posscats;
        for (int i=0; i < (int)st.stars.size(); ++i) {
            const StarSystem &star = st.stars[i];
            for (Color c = RED; c <= BLUE; ++c) {
                if (star.containsOverpopulation(c)) {
                    /* You might wonder why we're putting a whole std::string
                     * in each PossCat, instead of a pointer or an "int" index.
                     * The answer is that a star's name is the only reliable
                     * identifier once we start applying actions. Applying the
                     * first "catastrophe" action can cause a star to be removed,
                     * in which case the remaining stars shift down, invalidating
                     * pointers and indexes into the "st.stars" vector. Only the
                     * names are guaranteed to remain the same. */
                    posscats.push_back(PossCat(c, star.name));
                }
            }
        }
        /* For every combination of these catastrophes...
         * combine_precatastrophes() is the top of our recursive descent.
         * It bottoms out by calling combine_n_actions(), which calls
         * combine_one_action(), which calls combine_postcatastrophes(),
         * which finally bottoms out in a call to append_move(). Whee! */
        combine_precatastrophes(passmove, posscats, 0, st, attacker, all);
    } else {
        /* The original state contained no overpopulations, so we
         * can skip all that precatastrophe stuff. This is the
         * usual situation... unless our opponent set up an
         * overpopulation and then just left it there for us. */
        combine_normal(passmove, st, attacker, all);
    }

    if (prune_worse_moves && !all.seen.empty()) {
        /* We have found at least one move better than "pass". */
    } else {
        /* Just for completeness' sake, let's see if it's legal to make any
         * of the post-catastrophes available to us. If even doing that
         * would lose the game for us, then we'll do a plain vanilla "pass".
         * At this point, it's okay if we make a move that doesn't change the
         * state of the game; in fact, we actually expect to do so. */
        all.original_state = "";
        find_postcatastrophes(passmove, st, attacker, all);
        if (prune_worse_moves && !all.seen.empty()) {
            /* We have found at least one move better than "pass". */
        } else if (!look_only_for_wins) {
            /* "pass" can't be a winning move. */
            append_move(all, passmove, st, attacker);
        }
    }

#if ALLMOVES_USE_EXCEPTIONS
    } catch (...) { }
#endif
    return all.done;
}

static void combine_precatastrophes(const WholeMove &m,
    const std::vector<PossCat> &posscats, const int pc, const GameState &st,
    const int attacker, AllT &all)
{
    const int defender = 1-attacker;
    if (pc < (int)posscats.size()) {
        /* Get all the possible moves if we do catastrophe this color here. */
        Color color = posscats[pc].color;
        const StarSystem *where = st.systemNamed(posscats[pc].name.c_str());
        if (where == nullptr) {
            /* There was already a catastrophe at this star; it blew up
             * the entire star. Keep going, but obviously we can't
             * perform this catastrophe. */
            combine_precatastrophes(m, posscats, pc+1, st, attacker, all);
            return;
        }
        SingleAction newaction(CATASTROPHE, color, where->name.c_str());
        WholeMove newm(m, newaction);
        GameState newst = st;
        ApplyMove::or_die(newst, attacker, newaction);
        if (where->homeworldOf == attacker && newst.homeworldOf(attacker) == nullptr) {
            /* A catastrophe that blows up our own homeworld isn't allowed. */
        } else {
            assert(newst.homeworldOf(attacker) != nullptr);
            /* This catastrophe is allowed. Get all the possible moves if we do
             * catastrophe this color here. */
            if (all.prune_worse_moves &&
                where->homeworldOf == defender &&
                newst.homeworldOf(defender) == nullptr &&
                !newst.homeworldOf(attacker)->containsOverpopulation() &&
                !newst.hasLost(attacker)) {
                /* A catastrophe that blows up the defender's homeworld is
                 * an instant win... as long as there's no overpopulation
                 * threatening the attacker's homeworld!  If there *is*
                 * an overpopulation threatening us, we might need to
                 * make some non-trivial move (e.g. change color at home,
                 * or move in another ship) to fix it up. */
                append_move(all, newm, newst, attacker);
                assert(all.done);
                return;
            }
            if (where->homeworldOf == attacker && newst.hasLost(attacker)) {
                /* We just blew up the last ship at our homeworld. This is
                 * legal, but we'll need to move another ship in. */
                bool saved_look_for[4];
                memcpy(saved_look_for, all.look_for, sizeof all.look_for);
                all.look_for[RED] = false;
                all.look_for[GREEN] = false;
                all.look_for[BLUE] = false;
                combine_precatastrophes(newm, posscats, pc+1, newst, attacker, all);
                memcpy(all.look_for, saved_look_for, sizeof all.look_for);
            } else {
                combine_precatastrophes(newm, posscats, pc+1, newst, attacker, all);
            }
#if !ALLMOVES_USE_EXCEPTIONS
            if (all.done) return;
#endif
        }

        /* Also get all the possible moves if we don't catastrophe this color here. */
        combine_precatastrophes(m, posscats, pc+1, st, attacker, all);
        return;
    }
    /* Otherwise, we've reached the end of our list of potential
     * pre-catastrophes. Now look for proper moves --- either
     * moves we can make for free, or sacrifice moves. */
    combine_normal(m, st, attacker, all);
    return;
}

static void combine_normal(const WholeMove &m,
    const GameState &st, const int attacker, AllT &all)
{
    const int defender = 1-attacker;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &where = st.stars[i];
        if (all.win_only) {
            /* If we're just looking for winning moves, we know we don't
             * have to consider captures or builds outside the defender's
             * homeworld. We don't need to consider moves or color-changes
             * outside the defender's homeworld, unless there was an
             * overpopulation problem at home. */
            if (where.homeworldOf == defender) {
                if (all.look_for[RED] && where.playerHasAccessTo(attacker, RED)) {
                    combine_one_action(m, RED, 0, i, st, attacker, all);
                }
                if (all.contained_overpopulations && all.look_for[YELLOW] && where.playerHasAccessTo(attacker, YELLOW)) {
                    combine_one_action(m, YELLOW, 0, i, st, attacker, all);
                }
                if (all.look_for[GREEN] && where.playerHasAccessTo(attacker, GREEN)) {
                    combine_one_action(m, GREEN, 0, i, st, attacker, all);
                }
                if (all.look_for[BLUE] && where.playerHasAccessTo(attacker, BLUE)) {
                    combine_one_action(m, BLUE, 0, i, st, attacker, all);
                }
            } else {
                if (all.look_for[YELLOW] && where.playerHasAccessTo(attacker, YELLOW)) {
                    combine_one_action(m, YELLOW, 0, i, st, attacker, all);
                }
                if (all.contained_overpopulations && all.look_for[BLUE] && where.playerHasAccessTo(attacker, BLUE)) {
                    combine_one_action(m, BLUE, 0, i, st, attacker, all);
                }
            }
        } else {
            for (Color c = RED; c <= BLUE; ++c) {
                if (!all.look_for[c]) continue;
                if (!where.playerHasAccessTo(attacker, c)) continue;
                combine_one_action(m, c, 0, i, st, attacker, all);
            }
        }
#if !ALLMOVES_USE_EXCEPTIONS
        if (all.done) return;
#endif
    }
    /* Now we come to the part where the branching factor really kicks in:
     * sacrifices. For each ship of ours, find all the possible moves we
     * could make by sacrificing it.
     */
    for (const StarSystem& where : st.stars) {
        /* We can't sacrifice the last friendly ship at our homeworld,
         * unless it's yellow (in which case we'll make sure to move
         * another ship in before the end of the turn). */
        const bool last_friendly_ship = (where.homeworldOf == attacker &&
             where.ships[attacker].number() == 1);
        /* We can't sacrifice the very last ship at our own homeworld. */
        if (last_friendly_ship && where.ships[defender].empty()) {
            continue;
        }
        for (Color c = RED; c <= BLUE; ++c) {
            if (!all.look_for[c]) continue;
            if (last_friendly_ship && c != YELLOW) continue;
            for (Size s = SMALL; s <= LARGE; ++s) {
                /* Note that we needn't worry about duplicates; e.g., there's
                 * only one sacrifice to consider at "Alpha (y1) r2r2r2 -". */
                if (where.ships[attacker].numberOf(c,s) == 0) continue;
                SingleAction newaction(SACRIFICE, c,s, where.name.c_str());
                WholeMove newm(m, newaction);
                GameState newst = st;
                ApplyMove::or_die(newst, attacker, newaction);
                assert(newst.homeworldOf(attacker) != nullptr);
                const int num_moves = 1+(int)s;
                combine_n_actions(newm, c, num_moves, newst, attacker, all);
            }
        }
#if !ALLMOVES_USE_EXCEPTIONS
        if (all.done) return;
#endif
    }
    return;
}

static void combine_n_actions(const WholeMove &m, Color color, int num_moves,
    const GameState &st, int attacker, AllT &all)
{
    assert(1 <= num_moves && num_moves <= 3);
    /* We could just pass at this point (after sacrificing a ship and
     * taking all but "num_moves" of our available actions). */
    assert(st.homeworldOf(attacker) != nullptr);
    if (color != YELLOW || !st.homeworldOf(attacker)->ships[attacker].empty()) {
        finish_combine_one_action(m, color, 0, st, attacker, all);
    }
    /* Or we could take another of our available actions. */
    for (int i=0; i < (int)st.stars.size(); ++i) {
        combine_one_action(m, color, num_moves-1, i, st, attacker, all);
    }
}

static void combine_one_action(const WholeMove &m, Color color, int num_more_moves,
    int staridx, const GameState &st, int attacker, AllT &all)
{
    assert(all.look_for[color]);
    switch (color) {
        case RED:
            combine_one_red_action(m, num_more_moves, staridx, st, attacker, all);
            break;
        case YELLOW:
            combine_one_yellow_action(m, num_more_moves, staridx, st, attacker, all);
            break;
        case GREEN:
            combine_one_green_action(m, num_more_moves, staridx, st, attacker, all);
            break;
        case BLUE:
            combine_one_blue_action(m, num_more_moves, staridx, st, attacker, all);
            break;
        default: assert(false);
    }
}

static void combine_one_red_action(const WholeMove &m,
    const int num_more_moves, const int staridx, const GameState &st,
    const int attacker, AllT &all)
{
    const int defender = 1-attacker;
    const StarSystem &where = st.stars[staridx];
    /* Find all the possible ways of performing a single (free) RED
     * action at this system. In other words, find each enemy ship
     * here that we could possibly capture. */

    if (all.win_only) {
        /* Capturing away from the homeworld is pointless. */
        if (where.homeworldOf != defender) {
            return;
        }
    }

    if (where.ships[attacker].empty()) {
        return;
    }
    const Size maxsize = where.ships[attacker].biggestSize();
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= maxsize; ++s) {
            if (where.ships[defender].numberOf(c,s) == 0) continue;
            /* Note that a CAPTURE move has a "defender" in addition to the
             * "attacker", but in the two-player game the defender is just
             * "the other guy". */
            SingleAction newaction(CAPTURE, c,s, where.name.c_str());
            WholeMove newm(m, newaction);
            GameState newst = st;
            ApplyMove::or_die(newst, attacker, newaction);
            finish_combine_one_action(newm, RED, num_more_moves, newst, attacker, all);
        }
    }
}


static void combine_one_yellow_action(const WholeMove &m,
    const int num_more_moves, const int staridx, const GameState &st,
    const int attacker, AllT &all)
{
    const int defender = 1-attacker;
    const StarSystem &where = st.stars[staridx];
    /* Find all the possible ways of performing a single (free) YELLOW
     * action at this system. In other words, for each ship, find all the
     * stars we could move it to --- including newly created stars! */

    if (all.win_only && !all.contained_overpopulations) {
        /* Moving away from the defender's homeworld is pointless. */
        if (where.homeworldOf == defender) return;
    }

    const int num_friendly_ships_here = where.ships[attacker].number();
    if (num_friendly_ships_here == 0) {
        return;
    }

    /* But note that we can't move away the last ship from our own homeworld,
     * because as soon as you move the last ship away from a system, the
     * system disappears. Nor can we move the last friendly ship away from
     * our homeworld unless we have a way to replace it before the end of
     * the turn. */
    if (where.homeworldOf == attacker && num_friendly_ships_here == 1) {
        if (num_more_moves == 0) return;
        if (where.ships[defender].empty()) return;
    }

    /* If this is the last action of a yellow sacrifice move,
     * or if we started by blowing up all the friendly ships at
     * home, we need to make sure that we wind up with a ship
     * at our own homeworld. */
    assert(st.homeworldOf(attacker) != nullptr);
    const bool must_fly_homeward = (num_more_moves == 0 &&
            where.homeworldOf != attacker &&
            st.homeworldOf(attacker)->ships[attacker].empty());
    /* The last action of a winning move must (almost) always be directed
     * against the defender's homeworld. If we need to send a ship home
     * to replace a sacrificed yellow, as above, we can do that on the
     * second-to-last action without loss of generality.
     *   If we need to evacuate some ships to avoid an existing
     * overpopulation, then it's permissible to break this rule. */
    const bool maybe_need_to_evacuate =
            (all.contained_overpopulations && where.homeworldOf == attacker &&
             where.containsOverpopulation());
    const bool must_attack_defender = (num_more_moves == 0 && all.win_only && !maybe_need_to_evacuate);
    if (must_fly_homeward && must_attack_defender) {
        return;
    }

    /* We could move to another existing star... */
    for (int i=0; i < (int)st.stars.size(); ++i) {
        if (i == staridx) continue;
        const StarSystem &whither = st.stars[i];
        if (must_fly_homeward && whither.homeworldOf != attacker) {
            continue;
        }
        if (must_attack_defender && whither.homeworldOf != defender) {
            continue;
        }
        if (!whither.isAdjacentTo(where)) continue;
        /* Okay, now which ship shall we move there? */
        for (Color c = RED; c <= BLUE; ++c) {
            for (Size s = SMALL; s <= LARGE; ++s) {
                if (where.ships[attacker].numberOf(c,s) == 0) continue;
                SingleAction newaction(MOVE, c,s, where.name.c_str(), whither.name.c_str());
                WholeMove newm(m, newaction);
                GameState newst = st;
                ApplyMove::or_die(newst, attacker, newaction);
                assert(newst.homeworldOf(attacker) != nullptr);
                finish_combine_one_action(newm, YELLOW, num_more_moves, newst, attacker, all);
            }
        }
    }
    if (must_fly_homeward || must_attack_defender) {
        return;
    }
    if (all.done) {
        return;
    }
    /* Or we could move to a new star from the stash. */
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            if (where.ships[attacker].numberOf(c,s) == 0) continue;
            for (Size ns = SMALL; ns <= LARGE; ++ns) {
                if (where.star.numberOf(ns) != 0) continue;
                for (Color nc = RED; nc <= BLUE; ++nc) {
                    if (st.stash.numberOf(nc,ns) == 0) continue;
                    SingleAction newaction(MOVE_CREATE, c,s, where.name.c_str(), StarSystem::make_random_name(&st), nc,ns);
                    WholeMove newm(m, newaction);
                    GameState newst = st;
                    ApplyMove::or_die(newst, attacker, newaction);
                    assert(newst.homeworldOf(attacker) != nullptr);
                    finish_combine_one_action(newm, YELLOW, num_more_moves, newst, attacker, all);
                }
            }
        }
    }
}

static void combine_one_green_action(const WholeMove &m,
    const int num_more_moves, const int staridx, const GameState &st,
    const int attacker, AllT &all)
{
    const int defender = 1-attacker;
    const StarSystem &where = st.stars[staridx];
    /* Find all the possible ways of performing a single (free) GREEN
     * action at this system. In other words, for each color of ship we
     * have here, try to build a new ship of that color here. */

    if (all.win_only) {
        /* Building away from the defender's homeworld is pointless. */
        if (where.homeworldOf != defender) return;
    }

    for (Color c = RED; c <= BLUE; ++c) {
        if (where.ships[attacker].numberOf(c) == 0) continue;
        if (st.stash.numberOf(c) == 0) continue;
        const Size s = st.stash.smallestSizeOf(c);
        SingleAction newaction(BUILD, c,s, where.name.c_str());
        WholeMove newm(m, newaction);
        GameState newst = st;
        ApplyMove::or_die(newst, attacker, newaction);
        finish_combine_one_action(newm, GREEN, num_more_moves, newst, attacker, all);
    }
}

static void combine_one_blue_action(const WholeMove &m,
    const int num_more_moves, const int staridx, const GameState &st,
    const int attacker, AllT &all)
{
    const int defender = 1-attacker;
    const StarSystem &where = st.stars[staridx];
    /* Find all the possible ways of performing a single (free) BLUE
     * action at this system. In other words, for each ship we have here,
     * try exchanging it with every other ship of its size in the stash. */

    if (all.win_only && !all.contained_overpopulations) {
        /* It does make sense to trade a ship outside the defender's
         * homeworld, if we need that color to trade at the homeworld.
         * However, it doesn't make sense to do that as the *last* action. */
        if (num_more_moves == 0 && where.homeworldOf != defender) {
            return;
        }
    }

    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            if (where.ships[attacker].numberOf(c,s) == 0) continue;
            for (Color nc = RED; nc <= BLUE; ++nc) {
                if (nc == c) continue;
                if (st.stash.numberOf(nc,s) == 0) continue;
                SingleAction newaction(CONVERT, c,s, nc, where.name.c_str());
                WholeMove newm(m, newaction);
                GameState newst = st;
                ApplyMove::or_die(newst, attacker, newaction);
                finish_combine_one_action(newm, BLUE, num_more_moves, newst, attacker, all);
            }
        }
    }
}

static void finish_combine_one_action(const WholeMove &newm,
    const Color color, const int num_more_moves, const GameState &newst,
    const int attacker, AllT &all)
{
    if (num_more_moves != 0) {
        /* Go back and do some more free moves. */
        combine_n_actions(newm, color, num_more_moves, newst, attacker, all);
    } else {
        /* We're almost done with this move! Add post-catastrophes to the
         * current move. */
        find_postcatastrophes(newm, newst, attacker, all);
    }
}

static void find_postcatastrophes(const WholeMove &m,
    const GameState &st, const int attacker, AllT &all)
{
    /* Consider that to catastrophe any given overpopulation must give
     * an advantage either to the attacker or to the defender; either way,
     * one of the players is going to insist on it. Therefore, we might
     * as well catastrophe everything right now.
     *   There is one caveat here: What if *both* players' homeworlds are
     * susceptible to destruction? (This can't happen if both players
     * are playing perfectly.) Is the attacker allowed to claim victory
     * by choosing to catastrophe only his opponent's homeworld? According
     * to our "pre-catastrophe, post-catastrophe" model, it would be
     * allowed; however, according to the official rules, the defender
     * gets a chance to catastrophe the attacker's homeworld at the end
     * of the attacker's turn, so the attacker could not win.
     *   Since it actually simplifies the code, we'll follow the official
     * rules in this scenario; if both homeworlds are overpopulated, we'll
     * assume that it is not allowed to catastrophe just one of them.
     */
    std::vector<PossCat> posscats;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        const StarSystem &star = st.stars[i];
        for (Color c = RED; c <= BLUE; ++c) {
            if (star.containsOverpopulation(c)) {
                posscats.push_back(PossCat(c, star.name));
            }
        }
    }
    /* For every combination of these catastrophes... */
    combine_postcatastrophes(m, posscats, 0, st, attacker, all);
    return;
}

/* The vector "posscats" holds the names of stars where we can cause
 * catastrophes. As explained above, we normally don't need to consider
 * what happens if we make only some of these catastrophes, because if
 * we don't insist on making them, our opponent will.
 *   However, we do need to be careful that if one of these "catastrophe"
 * actions destroys a star system, we don't issue a second "catastrophe"
 * action on the same system.
 */
static void combine_postcatastrophes(const WholeMove &m,
    const std::vector<PossCat> &posscats, const int pc, const GameState &st,
    const int attacker, AllT &all)
{
    if (pc < (int)posscats.size()) {
        const Color color = posscats[pc].color;
        const StarSystem *where = st.systemNamed(posscats[pc].name.c_str());
        if (where == nullptr) {
            /* If a star has two possible post-catastrophes, then the
             * first catastrophe could blow up the whole star. */
            combine_postcatastrophes(m, posscats, pc+1, st, attacker, all);
            return;
        } else if (!all.prune_worse_moves) {
            /* Sure, our opponent will insist on making this catastrophe
             * if we don't, but we *are* still permitted not to make it. */
            combine_postcatastrophes(m, posscats, pc+1, st, attacker, all);
        }
        SingleAction newaction(CATASTROPHE, color, where->name.c_str());
        WholeMove newm(m, newaction);
        GameState newst = st;
        ApplyMove::or_die(newst, attacker, newaction);
        if (where->homeworldOf == attacker && newst.hasLost(attacker)) {
            /* A catastrophe that blows up our own homeworld isn't allowed.
             * We would recurse on combine_postcatastrophes() here, but the
             * defender would insist on this catastrophe anyway, so we might
             * as well abandon this whole move at this point. */
            return;
        }
        combine_postcatastrophes(newm, posscats, pc+1, newst, attacker, all);
        return;
    } else {
        /* Otherwise, we've reached the end of our list of potential
         * post-catastrophes. So we're done! */
        append_move(all, m, st, attacker);
        return;
    }
}

static void append_move(AllT &all, const WholeMove &m, const GameState &st,
    const int attacker)
{
#if !ALLMOVES_USE_EXCEPTIONS
    if (all.done) return;
#endif

    assert(!all.done);

    /* A move that results in the destruction of your own homeworld is
     * not permitted. We explicitly disallow sacrificing the last ship at
     * your homeworld, moving it away, or causing a catastrophe that
     * blows up your homeworld; so the homeworld should still be around. */
    assert(!st.hasLost(attacker));

    const bool is_win = st.hasLost(1-attacker);

    if (is_win) {
        /* Furthermore, we disallow "winning" moves that leave a fatal
         * overpopulation at your own homeworld.  This is because the
         * other player would get a chance to trigger that catastrophe
         * at the end of the "winning" turn, thus resulting in a
         * delayed destruction of the attacker's homeworld.  Check
         * for fatal overpopulations now. */
        GameState newst = st;
        newst.performAllCatastrophes();
        if (newst.hasLost(attacker)) {
            return;
        }
    }

    if (!is_win && all.win_only) {
        /* Sorry, we're only looking for winning moves. */
        return;
    } else {
        std::string key = st.toComparableString();
        if (key == all.original_state) {
            return;
        }
        if (all.emplace_into_seen(std::move(key))) {
            bool done = all.callback(m, st);
            if (done) {
                all.be_done();
            } else if (is_win && all.prune_worse_moves) {
                all.be_done();
            }
        }
    }
}
