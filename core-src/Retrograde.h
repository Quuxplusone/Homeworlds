#pragma once

#include <vector>

#include "WholeMove.h"
#include "state.h"

// Given the game state "st", where player "attacker" has just moved,
// call the given "callback" with each possible predecessor state (and one
// possible move that leads from that state to the current position).
// This code will never produce a predecessor state containing an
// overpopulation in any color.
//   The callback's signature should be bool(const WholeMove&, const GameState&).
// Our search will short-circuit as soon as the callback returns "true".
//
bool findAllRetrograde_impl(
    const GameState &st,
    int attacker,
    bool (*callback)(void*, const WholeMove&, const GameState&),
    void *callback_cookie);

template<class Callback>
bool findAllRetrograde(
    const GameState &st,
    int attacker,
    const Callback& callback)
{
    return findAllRetrograde_impl(
        st, attacker,
        +[](void *cookie, const WholeMove& m, const GameState& st) {
            const Callback& callback = *(const Callback*)cookie;
            return callback(m, st);
        },
        (void*)&callback
    );
}
