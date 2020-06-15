#pragma once

#include <vector>

#include "WholeMove.h"
#include "state.h"

extern bool debug;

// Given the game state "st", where player "attacker" has just moved,
// call the given "callback" with each possible predecessor state (and one
// possible move that leads from that state to the current position).
//   This code will produce predecessor states containing overpopulations
// only if "permit_overpopulations" is true. (This leads to unrealistic predecessor states.)
//   This code will never produce a move containing a catastrophe action,
// unless "permit_postcatastrophes" is true. (This causes a huge explosion of possibilities.)
//   The callback's signature should be bool(const WholeMove&, const GameState&).
// Our search will short-circuit as soon as the callback returns "true".
//
bool findAllRetrograde_impl(
    const GameState &st,
    int attacker,
    bool permit_overpopulations,
    bool permit_postcatastrophes,
    unsigned int these_colors_only,
    bool (*callback)(void*, const WholeMove&, const GameState&),
    void *callback_cookie);

template<class Callback>
bool findAllRetrograde(
    const GameState &st,
    int attacker,
    bool permit_overpopulations,
    bool permit_postcatastrophes,
    unsigned int these_colors_only,
    const Callback& callback)
{
    return findAllRetrograde_impl(
        st, attacker,
        permit_overpopulations,
        permit_postcatastrophes,
        these_colors_only,
        +[](void *cookie, const WholeMove& m, const GameState& st) {
            const Callback& callback = *(const Callback*)cookie;
            return callback(m, st);
        },
        (void*)&callback
    );
}
