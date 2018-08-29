#pragma once

#include <stdio.h>
#include <vector>
#include "state.h"
#include "GameStateEncoder.h"
#include "MillerNet.h"

struct GameStateEvaluator {
    Net net{{200, 100, 50, 1}};
    GameStateEncoder encoder;

    explicit GameStateEvaluator(const char *fn1, const char *fn2) {
        FILE *fp = fopen(fn1, "r");
        assert(fp != nullptr);
        encoder.load_from_file(fp);
        fclose(fp);
        fp = fopen(fn2, "r");
        assert(fp != nullptr);
        this->load_from_file(fp);
        fclose(fp);
    }

    void load_from_file(FILE *fp) {
        net.loadLayerFromFile(0, fp);
        net.loadLayerFromFile(1, fp);
    }

    bool is_worse_than(const GameState& a, const GameState& b) {
        net.feedForward(encoder.encode(a, b));
        std::vector<double> out;
        net.getResults(out);
        return (out[0] > 0.5);
    }
};
