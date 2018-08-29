#pragma once

#include <stdio.h>
#include <vector>
#include "state.h"
#include "State2Vec.h"
#include "MillerNet.h"

struct GameStateEncoder {
    Net net{{900, 600, 300, 100}};

    explicit GameStateEncoder() = default;

    void load_from_file(FILE *fp) {
        net.loadLayerFromFile(0, fp);
        net.loadLayerFromFile(1, fp);
        net.loadLayerFromFile(2, fp);
    }

    std::vector<double> encode(const GameState& st) {
        net.feedForward(State2VecVector(st).to_vector());
        std::vector<double> result;
        net.getResults(result);
        return result;
    }

    std::vector<double> encode(const GameState& a, const GameState& b) {
        return vec_concat(this->encode(a), this->encode(b));
    }
};
