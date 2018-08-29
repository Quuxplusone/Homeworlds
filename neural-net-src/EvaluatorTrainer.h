#pragma once

#include <memory>
#include <random>
#include <set>
#include <stdio.h>
#include <string>
#include <vector>
#include "GameStateEncoder.h"
#include "History.h"
#include "State2Vec.h"

struct EvaluatorTrainer {
    // [0]: States we have seen in real games where player 0 ended up winning.
    // [1]: States we have seen in real games where player 1 ended up winning.
    std::vector<Vector<100>> training_set[2];

    using NetType = Net<200, 20, 1>;
    std::unique_ptr<NetType> net;
    GameStateEncoder *encoder;

    EvaluatorTrainer(GameStateEncoder *e) {
        encoder = e;
        this->net = std::make_unique<NetType>();
    }

    void maybe_add_history_to_training_set(const History& h) {
        int winner = h.currentstate().hasLost(0);
        if (h.currentstate().hasLost(1-winner)) {
            // This game has a clear winner and a clear loser.
            for (int i=0; i < h.hidx; ++i) {
                const GameState& st = h.hvec[i+1].st;
                training_set[winner].push_back(encoder->encode(st));
                training_set[1-winner].push_back(encoder->encode(st.mirror()));
            }
        }
    }

    void train() {
        auto& winners = training_set[0];
        auto& losers = training_set[1];
        assert(winners.size() == losers.size());

        std::mt19937 g;
        printf("Training the evaluator on a set of %zu*%zu inputs...\n", winners.size(), losers.size());

        net->set_learning_rate(0.005f);
        const int n = winners.size();
        for (int round = 0; round < 10; ++round) {
            std::shuffle(winners.begin(), winners.end(), g);
            std::shuffle(losers.begin(), losers.end(), g);
            printf("...Round %d\n", round);
            for (int i=0; i < n; ++i) {
                train_single_position(winners[i].concat(losers[i]), 0.0f);
                train_single_position(losers[i].concat(winners[i]), 1.0f);
            }
            net->scale_learning_rate(0.98f);
        }
    }

    void train_single_position(const Vector<200>& in, float expected) {
        Vector<1> out;
        net->feed_forward(in, out);
        Vector<1> out_error = (out - expected);
        Vector<200> in_error;  // unused
        net->backpropagate_error(in_error, in, out, out_error);
    }

    void save_to_file(FILE *fp) const {
        net->layer1.save_to_file(fp);
        net->layer2.save_to_file(fp);
    }
};
