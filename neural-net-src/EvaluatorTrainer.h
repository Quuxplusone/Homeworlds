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
    std::vector<std::vector<double>> training_set[2];

    Net net{{200, 100, 50, 1}};
    GameStateEncoder *encoder;

    explicit EvaluatorTrainer(GameStateEncoder *e) {
        encoder = e;
    }

    static int identifyWinner(const GameState& st) {
        if (st.hasLost(0)) {
            if (!st.hasLost(1)) {
                return 1;
            }
        } else if (st.hasLost(1)) {
            return 0;
        }
        return -1;
    }

    void maybe_add_history_to_training_set(const History& h) {
        int winner = identifyWinner(h.currentstate());
        if (winner != -1) {
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

        const int n = winners.size();
        for (int round = 0; round < 10; ++round) {
            std::shuffle(winners.begin(), winners.end(), g);
            std::shuffle(losers.begin(), losers.end(), g);
            printf("...Round %d\n", round);
            for (int i=0; i < n; ++i) {
                train_single_position(vec_concat(winners[i], losers[i]), 0.0f);
                train_single_position(vec_concat(losers[i], winners[i]), 1.0f);
            }
            std::shuffle(winners.begin(), winners.end(), g);
            std::shuffle(losers.begin(), losers.end(), g);
            printf("...Testing round %d\n", round);
            int successes = 0;
            for (int i=0; i < n; ++i) {
                successes += test_single_position(vec_concat(winners[i], losers[i]), 0.0f);
                successes += test_single_position(vec_concat(losers[i], winners[i]), 1.0f);
            }
            printf("Success rate is %d/%d (%.2f%% correct)\n", successes, 2*n, 50. * successes / n);
        }
    }

    void train_single_position(const std::vector<double>& in, double expected) {
        net.feedForward(in);
        std::vector<double> out;
        net.getResults(out);
        net.backProp(std::vector<double>(1, expected));
    }

    int test_single_position(const std::vector<double>& in, double expected) {
        net.feedForward(in);
        std::vector<double> out;
        net.getResults(out);
        net.backProp(std::vector<double>(1, expected));
        return (out[0] > 0.5) == (expected > 0.5);
    }

    void save_to_file(FILE *fp) const {
        net.saveLayerToFile(0, fp);
        net.saveLayerToFile(1, fp);
    }
};
