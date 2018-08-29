#pragma once

#include <memory>
#include <random>
#include <set>
#include <stdio.h>
#include <string>
#include <vector>
#include "State2Vec.h"


struct GameStateEncoder {
    WeightsLayer<900, 600> layer1;
    Vector<600> hidden1;
    WeightsLayer<600, 300> layer2;
    Vector<300> hidden2;
    WeightsLayer<300, 100> layer3;

    void feed_forward(const Vector<900>& in, Vector<100>& out) {
        layer1.feed_forward(in, hidden1);
        layer2.feed_forward(hidden1, hidden2);
        layer3.feed_forward(hidden2, out);
    }

    void load_from_file(FILE *fp) {
        layer1.load_from_file(fp);
        layer2.load_from_file(fp);
        layer3.load_from_file(fp);
    }

    Vector<100> encode(const GameState& st) {
        State2VecVector vec(st);
        Vector<100> out;
        this->feed_forward(vec.to_vector(), out);
        return out;
    }
};
