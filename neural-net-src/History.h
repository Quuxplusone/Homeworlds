#pragma once

#include <assert.h>
#include <vector>
#include "move.h"
#include "state.h"
#include "ApplyMove.h"

struct History {
    struct Node {
        WholeMove move;
        GameState st;     /* the state *after* this move */
    };

    /* hvec[0] is the initial state; its "move" is garbage; it can't be undone. */
    std::vector<Node> hvec;
    /* hvec[hidx] is the last (non-undone) move. */
    int hidx;

    History(): hidx(0) {
        hvec.push_back(Node());
    }
    void setup(const GameState &st) {
        *this = History();
        assert(hidx == 0);
        assert(hvec.size() == 1);
        hvec[0].st = st;
    }
    void makemove(const WholeMove &move, int attacker) {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        assert(attacker == (hidx % 2));
        hvec.resize(hidx+1);
        hvec.push_back(hvec[hidx]);
        ++hidx;
        const bool UNUSED(success) = ApplyMove::Whole(hvec[hidx].st, attacker, move);
        assert(success);
        hvec[hidx].move = move;
    }
    const GameState &currentstate() const {
        return hvec[hidx].st;
    }
};
