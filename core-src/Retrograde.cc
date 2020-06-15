
#include <atomic>
#include <cassert>
#include <string>
#include <unordered_set>
#include <vector>
#include "combinations.h"
#include "state.h"
#include "Retrograde.h"
#include "WholeMove.h"

// Assume that each turn begins with zero overpopulations on the board.
// If we don't assume this, the number of predecessor states explodes dramatically;
// and we'll have to write some more code to handle the logic, anywhere you see this macro.
#define CATASTROPHES_ARE_MANDATORY 1

namespace {

using AllSeenT = std::unordered_set<std::string>;

struct AllT {
    std::string original_state;
    AllSeenT seen;
    bool (*callback)(void*, const WholeMove&, const GameState&);
    void *callback_cookie;
    bool done = false;
    bool permit_overpopulations = false;
    bool permit_postcatastrophes = false;
    unsigned int these_colors_only = 0xF;
    int attacker;
    int defender;

    explicit AllT(bool (*cb)(void*, const WholeMove&, const GameState&), void *cookie) :
        callback(cb),
        callback_cookie(cookie)
    {
    }

    std::vector<Color> count_postcatastrophe_material(const GameState& st) const;
    void look_for_postcatastrophes(const GameState& st, std::vector<SingleAction> actions, int catastrophes_left, const std::vector<Color>& posscats, int pc);
    void look_for_red_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions);
    void look_for_yellow_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions);
    void look_for_green_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions);
    void look_for_blue_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions);
    void look_for_sacrifice(Color color, const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions);
    void append_move(const std::vector<SingleAction>& actions, const GameState& st);

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

} // anonymous namespace

template<class It, class Callback>
static void for_each_combination2(It first, It mid, It last, const Callback& cb) {
    // An adaptor for Howard Hinnant's combination algorithm, to reduce the boilerplate.
    for_each_combination(first, mid, last, [&](auto, auto) {
        cb();
        return false;
    });
}

static std::string make_random_name(const GameState& st);

// Given the game state "st", report all possible last moves
// for player "attacker" via the "callback".
// As usual, don't report "pass" unless it is the only possible move.
//
bool findAllRetrograde_impl(const GameState &st, int attacker,
    bool permit_overpopulations,
    bool permit_postcatastrophes,
    unsigned int these_colors_only,
    bool (*callback)(void*, const WholeMove&, const GameState&),
    void *callback_cookie)
{
#if CATASTROPHES_ARE_MANDATORY
    assert(!st.containsOverpopulation());
#endif

    AllT all(callback, callback_cookie);
    all.original_state = st.toComparableString();
    all.permit_overpopulations = permit_overpopulations;
    all.permit_postcatastrophes = permit_postcatastrophes;
    all.these_colors_only = these_colors_only;
    all.attacker = attacker;
    all.defender = 1 - attacker;

#if ALLMOVES_USE_EXCEPTIONS
    try {
#endif

    if (all.permit_postcatastrophes) {
#if CATASTROPHES_ARE_MANDATORY
        // If the state can't contain any overpopulations at the start of the turn,
        // then the number of postcatastrophes is limited to the number of overpopulations
        // we can create in a single turn via yellow, green, and blue actions;
        // i.e., at most three overpopulations, and they must all involve at least
        // one of the attacker's own ships.
        // This still results in a huge explosion of possible states.
        auto posscats = all.count_postcatastrophe_material(st);
        all.look_for_postcatastrophes(st, {}, 3, posscats, 0);
#endif
    } else {
        all.look_for_postcatastrophes(st, {}, 0, {}, 0);
    }

    if (all.seen.empty()) {
        all.append_move({}, st);
    }

#if ALLMOVES_USE_EXCEPTIONS
    } catch (...) { }
#endif
    return all.done;
}

std::vector<Color> AllT::count_postcatastrophe_material(const GameState& st) const
{
    std::vector<Color> result;
    for (Color c = RED; c <= BLUE; ++c) {
        int count = st.stash.numberOf(c);
        assert(0 <= count && count <= 9);
        if (count >= 4) {
            result.push_back(c);
        }
    }
    return result;
}

void AllT::look_for_postcatastrophes(const GameState& st, std::vector<SingleAction> actions, int catastrophes_left,
                                     const std::vector<Color>& posscats, int pc)
{
    if (pc == int(posscats.size())) {
        if (this->these_colors_only & (1 << RED)) {
            if (this->permit_overpopulations || !st.containsOverpopulation()) {
                this->look_for_red_moves(st, actions, 0);
            }
        }
        if (this->these_colors_only & (1 << YELLOW)) {
            this->look_for_yellow_moves(st, actions, 0);
        }
        if (this->these_colors_only & (1 << GREEN)) {
            this->look_for_green_moves(st, actions, 0);
        }
        if (this->these_colors_only & (1 << BLUE)) {
            this->look_for_blue_moves(st, actions, 0);
        }
        return;
    }

    Color c = posscats[pc];
    if (catastrophes_left > 0 && st.stash.numberOf(c) >= 4) {
        // Consider a catastrophe in this color at a star that is still extant.
        // Since it didn't eliminate the star, the only pieces it eliminated must be of the catastrophe color.
        for (const StarSystem& where : st.stars) {
            if (where.numberOf(c) != 0) continue;
            GameState prevst = st;
            StarSystem *prevwhere = prevst.systemNamed(where.name.c_str());
            std::vector<Piece> pieces;
            for (Size s = SMALL; s <= LARGE; ++s) {
                pieces.insert(pieces.end(), prevst.stash.numberOf(c, s), Piece(c, s));
            }
            bool consider_star_itself = (prevwhere->homeworldOf != -1 && prevwhere->star.number() == 1);
            // Without "permit_overpopulations", we can't involve more pieces in this catastrophe
            // than the 3 that were already there, plus up to 3 pieces brought in by the attacker's actions.
            int max = (permit_overpopulations ? pieces.size() : 6);
            int min_attackers = (permit_overpopulations ? 0 : 1);
            for (int num_pieces = 4; num_pieces < max; ++num_pieces) {
                for_each_combination2(pieces.begin(), pieces.begin() + num_pieces, pieces.end(), [&]() {
                    // These pieces are involved. Which ones are the attacker's,
                    // which are the defender's, and which is part of the star?
                    if (consider_star_itself) {
                        for_each_combination2(pieces.begin(), pieces.begin() + 1, pieces.end(), [&]() {
                            Piece star = *pieces.begin();
                            for (int num_defenders = 0; num_defenders <= num_pieces - min_attackers - 1; ++num_defenders) {
                                for_each_combination2(pieces.begin() + 1, pieces.begin() + 1 + num_defenders, pieces.end(), [&]() {
                                    StarSystem old = *prevwhere;
                                    prevwhere->star += star;
                                    std::for_each(pieces.begin() + 1, pieces.begin() + 1 + num_defenders, [&](Piece p) {
                                        prevwhere->ships[defender] += p;
                                    });
                                    std::for_each(pieces.begin() + 1 + num_defenders, pieces.end(), [&](Piece p) {
                                        prevwhere->ships[attacker] += p;
                                    });
                                    actions.emplace_back(CATASTROPHE, c, prevwhere->name.c_str());
                                    this->look_for_postcatastrophes(prevst, actions, catastrophes_left - 1, posscats, pc);
                                    actions.pop_back();
                                    *prevwhere = old;
                                });
                            }
                        });
                    }
                    for (int num_defenders = 0; num_defenders <= num_pieces - min_attackers; ++num_defenders) {
                        for_each_combination2(pieces.begin(), pieces.begin() + num_defenders, pieces.end(), [&]() {
                            StarSystem old = *prevwhere;
                            std::for_each(pieces.begin(), pieces.begin() + num_defenders, [&](Piece p) {
                                prevwhere->ships[defender] += p;
                            });
                            std::for_each(pieces.begin() + num_defenders, pieces.end(), [&](Piece p) {
                                prevwhere->ships[attacker] += p;
                            });
                            actions.emplace_back(CATASTROPHE, c, prevwhere->name.c_str());
                            this->look_for_postcatastrophes(prevst, actions, catastrophes_left - 1, posscats, pc);
                            actions.pop_back();
                            *prevwhere = old;
                        });
                    }
                });
            }
        }

        // Consider a catastrophe in this color at a star that is now in the stash.
        // Since the whole star was eliminated, it could have had additional pieces of any color,
        // posing as either the attacker's or the defender's ships.
        std::string whencename = make_random_name(st);
        actions.emplace_back(CATASTROPHE, c, whencename.c_str());
        for (Size s = SMALL; s <= LARGE; ++s) {
            Piece star = Piece(c, s);
            if (!st.stash.contains(star)) continue;
            GameState prevst = st;
            prevst.stash -= star;
            std::vector<Piece> pieces;
            for (Piece p : Piece::possibilities()) {
                auto pos = (p.color == c) ? pieces.begin() : pieces.end();
                pieces.insert(pos, prevst.stash.numberOf(p), p);
            }
            auto pieces_end_of_c = std::partition_point(pieces.begin(), pieces.end(), [&](Piece p){ return p.color == c; });
            assert(pieces_end_of_c - pieces.begin() >= 3);
            prevst.stars.push_back(StarSystem(whencename.c_str()));
            StarSystem& prevwhence = prevst.stars.back();
            prevwhence.star += star;
            // Pick 3 other pieces of the catastrophe color to be ships at this system.
            // At least one of these ships must be the attacker's.
            for_each_combination2(pieces.begin(), pieces.begin() + 1, pieces_end_of_c, [&]() {
                prevwhence.ships[attacker] += pieces[0];
                for_each_combination2(pieces.begin() + 1, pieces.begin() + 3, pieces_end_of_c, [&]() {
                    for (int mask = 0; mask < 4; ++mask) {
                        prevwhence.ships[(mask & 1) ? attacker : defender] += pieces[1];
                        prevwhence.ships[(mask & 2) ? attacker : defender] += pieces[2];
                        // Pick as many other pieces as you like.
                        for (int i = 3; i <= int(pieces.size()); ++i) {
                            for_each_combination2(pieces.begin() + 3, pieces.begin() + i, pieces.end(), [&]() {
                                for (int mask = 0; mask < (1 << (i-3)); ++mask) {
                                    for (int j=0; j < (i-3); ++j) {
                                        prevwhence.ships[(mask & (1 << j)) ? attacker : defender] += pieces[3+j];
                                    }
                                    this->look_for_postcatastrophes(prevst, actions, catastrophes_left - 1, posscats, pc);
                                    for (int j=0; j < (i-3); ++j) {
                                        prevwhence.ships[(mask & (1 << j)) ? attacker : defender] -= pieces[3+j];
                                    }
                                }
                            });
                        }
                        prevwhence.ships[(mask & 1) ? attacker : defender] -= pieces[1];
                        prevwhence.ships[(mask & 2) ? attacker : defender] -= pieces[2];
                    }
                });
                prevwhence.ships[attacker] -= pieces[0];
            });
        }
        actions.pop_back();
    }

    // Also consider NO catastrophe in this color.
    this->look_for_postcatastrophes(st, actions, catastrophes_left, posscats, pc + 1);
}

void AllT::look_for_red_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions)
{
    this->look_for_sacrifice(RED, st, actions, sacrifice_actions);
    if (sacrifice_actions == 3) {
        return;
    }

    for (const StarSystem& where : st.stars) {
        if (where.ships[attacker].number() >= 2) {
            const Size maxsize = where.ships[attacker].biggestSize();
            for (Piece ship : Piece::possibilities()) {
                if (ship.size > maxsize) continue;
                if (where.ships[attacker].numberOf(ship) == 0) continue;
                GameState prevst = st;
                StarSystem *prevwhere = prevst.systemNamed(where.name.c_str());
                prevwhere->ships[attacker] -= ship;
                if (prevwhere->ships[attacker].biggestSize() < ship.size) continue;
                bool requires_red_sacrifice = !prevwhere->playerHasAccessTo(attacker, RED);
                prevwhere->ships[defender] += ship;

                actions.emplace_back(CAPTURE, ship, where.name.c_str());
                if (sacrifice_actions == 0 && !requires_red_sacrifice) {
                    // This could be a free action!
                    this->append_move(actions, prevst);
                }
                this->look_for_red_moves(prevst, actions, sacrifice_actions + 1);
                actions.pop_back();
            }
        }
    }
}

void AllT::look_for_yellow_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions)
{
    this->look_for_sacrifice(YELLOW, st, actions, sacrifice_actions);
    if (sacrifice_actions == 3) {
        return;
    }

    for (const StarSystem& where : st.stars) {
        if (where.ships[attacker].number() == 0) {
            // do nothing
        } else if (where.ships[attacker].number() == 1 && where.ships[1 - attacker].number() == 0) {
            if (where.homeworldOf != -1) {
                // A homeworld can't have been discovered this turn.
            } else {
                // This system could have been discovered this turn.
                Piece ship = where.ships[attacker].onlyPiece();
                Piece star = where.star.onlyPiece();
                GameState prevst = st;
                prevst.removeSystemNamed(where.name.c_str());
                prevst.stash += star;

                // This ship could have moved from a star that is still extant.
                for (StarSystem& prevwhence : prevst.stars) {
                    if (!prevwhence.isAdjacentTo(where)) continue;
                    // The ship could have departed from here.
                    bool requires_yellow_sacrifice = (ship.color != YELLOW && !prevwhence.playerHasAccessTo(attacker, YELLOW));
                    prevwhence.ships[attacker] += ship;
                    actions.emplace_back(MOVE_CREATE, ship, prevwhence.name.c_str(), where.name.c_str(), star);
                    if (sacrifice_actions == 0 && !requires_yellow_sacrifice) {
                        // This could be a free action!
                        this->append_move(actions, prevst);
                    }
                    this->look_for_yellow_moves(prevst, actions, sacrifice_actions + 1);
                    actions.pop_back();
                    prevwhence.ships[attacker] -= ship;
                }

                // This ship could have moved from a star that is now in the stash.
                std::string whencename = make_random_name(st);
                for (Piece whencestar : Piece::possibilities()) {
                    if (prevst.stash.contains(whencestar) && where.isAdjacentTo(whencestar)) {
                        // The ship could have departed from here.
                        bool requires_yellow_sacrifice = (ship.color != YELLOW && whencestar.color != YELLOW);
                        prevst.stash -= whencestar;
                        prevst.stars.push_back(StarSystem(whencename.c_str()));
                        prevst.stars.back().star += whencestar;
                        prevst.stars.back().ships[attacker] += ship;
                        actions.emplace_back(MOVE_CREATE, ship, whencename.c_str(), where.name.c_str(), star);
                        if (sacrifice_actions == 0 && !requires_yellow_sacrifice) {
                            // This could be a free action!
                            this->append_move(actions, prevst);
                        }
                        this->look_for_yellow_moves(prevst, actions, sacrifice_actions + 1);
                        actions.pop_back();
                        prevst.stars.pop_back();
                        prevst.stash += whencestar;
                    }
                }
            }
        } else {
            for (Piece ship : Piece::possibilities()) {
                if (!where.ships[attacker].contains(ship)) continue;
                GameState prevst = st;
                prevst.systemNamed(where.name.c_str())->ships[attacker] -= ship;

                // This ship could have moved from a star that is still extant.
                for (StarSystem& prevwhence : prevst.stars) {
                    if (!prevwhence.isAdjacentTo(where)) continue;
                    // The ship could have departed from here.
                    bool requires_yellow_sacrifice = (ship.color != YELLOW && !prevwhence.playerHasAccessTo(attacker, YELLOW));
                    prevwhence.ships[attacker] += ship;
                    actions.emplace_back(MOVE, ship, prevwhence.name.c_str(), where.name.c_str());
                    if (sacrifice_actions == 0 && !requires_yellow_sacrifice) {
                        // This could be a free action!
                        this->append_move(actions, prevst);
                    }
                    this->look_for_yellow_moves(prevst, actions, sacrifice_actions + 1);
                    actions.pop_back();
                    prevwhence.ships[attacker] -= ship;
                }

                // This ship could have moved from a star that is now in the stash.
                std::string whencename = make_random_name(st);
                for (Piece whencestar : Piece::possibilities()) {
                    if (prevst.stash.contains(whencestar) && where.isAdjacentTo(whencestar)) {
                        // The ship could have departed from here.
                        bool requires_yellow_sacrifice = (ship.color != YELLOW && whencestar.color != YELLOW);
                        prevst.stash -= whencestar;
                        prevst.stars.push_back(StarSystem(whencename.c_str()));
                        prevst.stars.back().star += whencestar;
                        prevst.stars.back().ships[attacker] += ship;
                        actions.emplace_back(MOVE, ship, whencename.c_str(), where.name.c_str());
                        if (sacrifice_actions == 0 && !requires_yellow_sacrifice) {
                            // This could be a free action!
                            this->append_move(actions, prevst);
                        }
                        this->look_for_yellow_moves(prevst, actions, sacrifice_actions + 1);
                        actions.pop_back();
                        prevst.stars.pop_back();
                        prevst.stash += whencestar;
                    }
                }
            }
        }
    }
}

void AllT::look_for_green_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions)
{
    this->look_for_sacrifice(GREEN, st, actions, sacrifice_actions);
    if (sacrifice_actions == 3) {
        return;
    }

    for (const StarSystem& where : st.stars) {
        bool requires_green_sacrifice = !where.playerHasAccessTo(attacker, GREEN);
        if (where.ships[attacker].number() >= 2) {
            for (Color c = RED; c <= BLUE; ++c) {
                if (where.ships[attacker].numberOf(c) < 2) continue;
                for (Size s = SMALL; s <= LARGE; ++s) {
                    if (where.ships[attacker].numberOf(c, s) == 0) continue;
                    GameState prevst = st;
                    StarSystem *prevwhere = prevst.systemNamed(where.name.c_str());
                    prevst.stash += Piece(c, s);
                    if (prevst.stash.smallestSizeOf(c) != s) {
                        continue;
                    }
                    prevwhere->ships[attacker] -= Piece(c, s);
                    actions.emplace_back(BUILD, Piece(c, s), where.name.c_str());
                    if (sacrifice_actions == 0 && !requires_green_sacrifice) {
                        // This could be a free action!
                        this->append_move(actions, prevst);
                    }
                    this->look_for_green_moves(prevst, actions, sacrifice_actions + 1);
                    actions.pop_back();
                }
            }
        }
    }
}

void AllT::look_for_blue_moves(const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions)
{
    this->look_for_sacrifice(BLUE, st, actions, sacrifice_actions);
    if (sacrifice_actions == 3) {
        return;
    }

    GameState prevst = st;
    for (const StarSystem& where : st.stars) {
        for (Piece ship : Piece::possibilities()) {
            if (where.ships[attacker].numberOf(ship) == 0) continue;
            // This ship could be the result of a swap. What's in the stash?
            StarSystem *prevwhere = prevst.systemNamed(where.name.c_str());
            prevwhere->ships[attacker] -= ship;
            prevst.stash += ship;
            for (Color oc = RED; oc <= BLUE; ++oc) {
                if (oc == ship.color) continue;
                Piece oldship = Piece(oc, ship.size);
                if (st.stash.numberOf(oldship) != 0) {
                    prevst.stash -= oldship;
                    prevwhere->ships[attacker] += oldship;
                    bool requires_blue_sacrifice = !prevwhere->playerHasAccessTo(attacker, BLUE);
                    actions.emplace_back(CONVERT, oldship, ship, where.name.c_str());
                    if (sacrifice_actions == 0 && !requires_blue_sacrifice) {
                        // This could be a free action!
                        this->append_move(actions, prevst);
                    }
                    this->look_for_blue_moves(prevst, actions, sacrifice_actions + 1);
                    actions.pop_back();
                    prevwhere->ships[attacker] -= oldship;
                    prevst.stash += oldship;
                }
            }
            prevwhere->ships[attacker] += ship;
            prevst.stash -= ship;
        }
    }
}

void AllT::look_for_sacrifice(Color color, const GameState& st, std::vector<SingleAction> actions, int sacrifice_actions)
{
    if (!this->permit_overpopulations && st.containsOverpopulation()) {
        // Adding a sacrificed ship on the front of this move won't fix the problem.
        return;
    }

    GameState prevst = st;
    for (Piece ship : Piece::possibilities()) {
        if (ship.color != color) continue;
        if (ship.sacrificeActions() < sacrifice_actions) continue;
        if (!prevst.stash.contains(ship)) continue;

        prevst.stash -= ship;

        // This ship could have been sacced at a star that is still extant.
        for (StarSystem& where : prevst.stars) {
            where.ships[attacker] += ship;
            actions.push_back(SingleAction(SACRIFICE, ship, where.name.c_str()));
            this->append_move(actions, prevst);
            actions.pop_back();
            where.ships[attacker] -= ship;
        }

        // This ship could have been sacced at a star that is now in the stash.
        std::string whencename = make_random_name(st);
        for (Piece whencestar : Piece::possibilities()) {
            if (prevst.stash.contains(whencestar)) {
                prevst.stash -= whencestar;
                prevst.stars.push_back(StarSystem(whencename.c_str()));
                prevst.stars.back().star += whencestar;
                prevst.stars.back().ships[attacker] += ship;
                actions.push_back(SingleAction(SACRIFICE, ship, whencename.c_str()));
                this->append_move(actions, prevst);
                actions.pop_back();
                prevst.stars.pop_back();
                prevst.stash += whencestar;
            }
        }

        prevst.stash += ship;
    }
}

static std::string make_random_name(const GameState& st)
{
    char buffer[7] = "UuXXXX";
    static std::atomic<unsigned int> atomic_counter{0};
    unsigned int counter = atomic_counter.fetch_add(1, std::memory_order_relaxed);
    do {
        buffer[2] = 'a' + (counter & 0xF);
        buffer[3] = 'a' + ((counter >> 4) & 0xF);
        buffer[4] = 'a' + ((counter >> 8) & 0xF);
        buffer[5] = 'a' + ((counter >> 12) & 0xF);
        assert(buffer[6] == '\0');
        ++counter;
        /* If there's already a star with this name, just increment the
         * counter and try again. There certainly can't be 65536 stars
         * in the galaxy; there aren't that many pieces in the stash!
         */
    } while (st.systemNamed(buffer) != nullptr);
    assert(StarSystem::isValidName(buffer));
    return buffer;
}

void AllT::append_move(const std::vector<SingleAction>& actions, const GameState &st)
{
#if !ALLMOVES_USE_EXCEPTIONS
    if (this->done) return;
#endif

    assert(!this->done);

    // The preceding state can't have been "game over."
    if (st.hasLost(attacker) || st.hasLost(defender)) {
        return;
    }

    if ((!this->permit_overpopulations) && st.containsOverpopulation()) {
        return;
    }

    std::string key = st.toComparableString();
    if (key == this->original_state) {
        return;
    }
    if (this->emplace_into_seen(std::move(key))) {
        WholeMove m;
        for (auto it = actions.rbegin(); it != actions.rend(); ++it) {
            m = WholeMove(m, *it);
        }

        if (this->callback(this->callback_cookie, m, st)) {
            this->be_done();
        }
    }
}
