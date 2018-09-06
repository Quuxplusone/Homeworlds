#pragma once

#include <stdio.h>
#include <vector>
#include "state.h"
#include "MillerNet.h"
#include "State2Vec.h"

struct GameStateEncoder {
    Net net{{900, 600, 300, 100}};

    explicit GameStateEncoder() = default;

    void load_from_file(FILE *fp) {
        net.loadLayerFromFile(0, fp);
        net.loadLayerFromFile(1, fp);
        net.loadLayerFromFile(2, fp);
    }

    std::vector<REAL> encode(const State2VecVector& vec) {
        net.feedForward(vec);
        return net.getResults();
    }

    std::vector<REAL> encode(const GameState& st, int who_just_moved) {
        return this->encode(State2VecVector(st, who_just_moved));
    }

    std::vector<REAL> encode(const State2VecVector& a, const State2VecVector& b) {
        return vec_concat(this->encode(a), this->encode(b));
    }

    std::vector<REAL> encode(const GameState& a, const GameState& b, int who_just_moved) {
        return this->encode(State2VecVector(a, who_just_moved), State2VecVector(b, who_just_moved));
    }
};
