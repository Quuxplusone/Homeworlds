
#include <assert.h>
#include <string.h>
#include <atomic>
#include <string>
#include <unordered_set>
#include <vector>
#include "state.h"
#include "Retrograde.h"
#include "WholeMove.h"

namespace {

using AllSeenT = std::unordered_set<std::string>;

struct AllT {
    std::string original_state;
    AllSeenT seen;
    bool (*callback)(void*, const WholeMove&, const GameState&);
    void *callback_cookie;
    bool done = false;

    explicit AllT(bool (*cb)(void*, const WholeMove&, const GameState&), void *cookie) :
        callback(cb),
        callback_cookie(cookie)
    {
    }

    void look_for_free_red_moves(const GameState& st, int attacker);
    void look_for_free_yellow_moves(const GameState& st, int attacker);
    void look_for_free_green_moves(const GameState& st, int attacker);
    void look_for_free_blue_moves(const GameState& st, int attacker);
    void look_for_pointless_sacrifices(const GameState& st, int attacker);
    void append_move(const WholeMove& m, const GameState& st, int attacker);

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

static std::string make_random_name(const GameState& st);

// Given the game state "st", report all possible last moves
// for player "attacker" via the "callback".
// As usual, don't report "pass" unless it is the only possible move.
//
bool findAllRetrograde_impl(const GameState &st, int attacker,
    bool (*callback)(void*, const WholeMove&, const GameState&),
    void *callback_cookie)
{
    AllT all(callback, callback_cookie);
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

    all.look_for_free_red_moves(st, attacker);
    all.look_for_free_yellow_moves(st, attacker);
    all.look_for_free_green_moves(st, attacker);
    all.look_for_free_blue_moves(st, attacker);
    all.look_for_pointless_sacrifices(st, attacker);

    if (all.seen.empty()) {
        WholeMove passmove;
        all.append_move(passmove, st, attacker);
    }

#if ALLMOVES_USE_EXCEPTIONS
    } catch (...) { }
#endif
    return all.done;
}

void AllT::look_for_free_red_moves(const GameState& st, int attacker)
{
    int defender = 1 - attacker;
    for (const StarSystem& where : st.stars) {
        if (where.ships[attacker].number() >= 2 && where.playerHasAccessTo(attacker, RED)) {
            const Size maxsize = where.ships[attacker].biggestSize();
            for (Color c = RED; c <= BLUE; ++c) {
                for (Size s = SMALL; s <= maxsize; ++s) {
                    if (where.ships[attacker].numberOf(c,s) == 0) continue;
                    GameState prevst = st;
                    StarSystem *prevwhere = prevst.systemNamed(where.name.c_str());
                    prevwhere->ships[attacker] -= Piece(c, s);
                    if (prevwhere->ships[attacker].biggestSize() < s || !prevwhere->playerHasAccessTo(attacker, RED)) {
                        continue;
                    }
                    prevwhere->ships[defender] += Piece(c, s);
                    SingleAction newaction(CAPTURE, Piece(c, s), where.name.c_str());
                    WholeMove m(WholeMove{}, newaction);
                    this->append_move(m, prevst, attacker);
                }
            }
        }
    }
}

void AllT::look_for_free_yellow_moves(const GameState& st, int attacker)
{
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
                prevst.stash += where.pieceCollection();
                prevst.removeSystemNamed(where.name.c_str());

                // This ship could have moved from a star that is still extant.
                for (StarSystem& prevwhence : prevst.stars) {
                    if (!prevwhence.isAdjacentTo(where)) continue;
                    if (ship.color == YELLOW || prevwhence.playerHasAccessTo(attacker, YELLOW)) {
                        // The ship could have departed from here.
                        if (prevwhence.numberOf(ship.color) == 3) {
                            // We assume that the preceding state can't have contained any overpopulations.
                            continue;
                        }
                        prevwhence.ships[attacker] += ship;
                        SingleAction newaction(MOVE_CREATE, ship, prevwhence.name.c_str(), where.name.c_str(), star);
                        this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
                        prevwhence.ships[attacker] -= ship;
                    }
                }

                // This ship could have moved from a star that is now in the stash.
                std::string whencename = make_random_name(st);
                for (Piece whencestar : Piece::possibilities()) {
                    if (prevst.stash.contains(whencestar) && where.isAdjacentTo(whencestar)) {
                        if (whencestar.color == YELLOW || ship.color == YELLOW) {
                            // The ship could have departed from here.
                            prevst.stash -= whencestar;
                            prevst.stars.push_back(StarSystem(whencename.c_str()));
                            prevst.stars.back().star += whencestar;
                            prevst.stars.back().ships[attacker] += ship;
                            SingleAction newaction(MOVE_CREATE, ship, whencename.c_str(), where.name.c_str(), star);
                            this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
                            prevst.stars.pop_back();
                            prevst.stash += whencestar;
                        }
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
                    if (ship.color == YELLOW || prevwhence.playerHasAccessTo(attacker, YELLOW)) {
                        // The ship could have departed from here.
                        if (prevwhence.numberOf(ship.color) == 3) {
                            // We assume that the preceding state can't have contained any overpopulations.
                            continue;
                        }
                        prevwhence.ships[attacker] += ship;
                        SingleAction newaction(MOVE, ship, prevwhence.name.c_str(), where.name.c_str());
                        this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
                        prevwhence.ships[attacker] -= ship;
                    }
                }

                // This ship could have moved from a star that is now in the stash.
                std::string whencename = make_random_name(st);
                for (Piece whencestar : Piece::possibilities()) {
                    if (prevst.stash.contains(whencestar) && where.isAdjacentTo(whencestar)) {
                        if (whencestar.color == YELLOW || ship.color == YELLOW) {
                            // The ship could have departed from here.
                            prevst.stash -= whencestar;
                            prevst.stars.push_back(StarSystem(whencename.c_str()));
                            prevst.stars.back().star += whencestar;
                            prevst.stars.back().ships[attacker] += ship;
                            SingleAction newaction(MOVE, ship, whencename.c_str(), where.name.c_str());
                            this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
                            prevst.stars.pop_back();
                            prevst.stash += whencestar;
                        }
                    }
                }
            }
        }
    }
}

void AllT::look_for_free_green_moves(const GameState& st, int attacker)
{
    for (const StarSystem& where : st.stars) {
        if (where.ships[attacker].number() >= 2 && where.playerHasAccessTo(attacker, GREEN)) {
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
                    SingleAction newaction(BUILD, Piece(c, s), where.name.c_str());
                    WholeMove m(WholeMove{}, newaction);
                    this->append_move(m, prevst, attacker);
                }
            }
        }
    }
}

void AllT::look_for_free_blue_moves(const GameState& st, int attacker)
{
    GameState prevst = st;
    for (const StarSystem& where : st.stars) {
        for (Piece ship : Piece::possibilities()) {
            if (where.ships[attacker].numberOf(ship) == 0) continue;
            // This ship could be the result of a swap. What's in the stash?
            StarSystem *prevwhere = prevst.systemNamed(where.name.c_str());
            prevwhere->ships[attacker] -= ship;
            prevst.stash += ship;
            bool old_ship_must_have_been_blue = !prevwhere->playerHasAccessTo(attacker, BLUE);
            for (Color oc = RED; oc <= BLUE; ++oc) {
                if (old_ship_must_have_been_blue && oc != BLUE) continue;
                if (oc == ship.color) continue;
                if (prevwhere->numberOf(oc) == 3) {
                    // We assume that the preceding state can't have contained any overpopulations.
                    continue;
                }
                Piece oldship = Piece(oc, ship.size);
                if (st.stash.numberOf(oldship) != 0) {
                    prevst.stash -= oldship;
                    prevwhere->ships[attacker] += oldship;
                    SingleAction newaction(CONVERT, oldship, ship, where.name.c_str());
                    this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
                    prevwhere->ships[attacker] -= oldship;
                    prevst.stash += oldship;
                }
            }
            prevwhere->ships[attacker] += ship;
            prevst.stash -= ship;
        }
    }
}

void AllT::look_for_pointless_sacrifices(const GameState& st, int attacker)
{
    GameState prevst = st;
    for (Piece ship : Piece::possibilities()) {
        if (prevst.stash.contains(ship)) {
            prevst.stash -= ship;

            // This ship could have been sacced at a star that is still extant.
            for (StarSystem& where : prevst.stars) {
                if (where.pieceCollection().numberOf(ship.color) == 3) continue;
                where.ships[attacker] += ship;
                SingleAction newaction(SACRIFICE, ship, where.name.c_str());
                this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
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
                    SingleAction newaction(SACRIFICE, ship, whencename.c_str());
                    this->append_move(WholeMove(WholeMove{}, newaction), prevst, attacker);
                    prevst.stars.pop_back();
                    prevst.stash += whencestar;
                }
            }

            prevst.stash += ship;
        }
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

void AllT::append_move(const WholeMove &m, const GameState &st, int attacker)
{
#if !ALLMOVES_USE_EXCEPTIONS
    if (this->done) return;
#endif

    assert(!this->done);

    // The preceding state can't have been "game over."
    assert(!st.hasLost(attacker));
    assert(!st.hasLost(1 - attacker));

    std::string key = st.toComparableString();
    if (key == this->original_state) {
        return;
    }
    if (this->emplace_into_seen(std::move(key))) {
        if (this->callback(this->callback_cookie, m, st)) {
            this->be_done();
        }
    }
}
