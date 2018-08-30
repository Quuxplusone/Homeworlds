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
    std::vector<std::vector<REAL>> mix_and_match_training_set[2];

    // [0]: (Before, after) states from real games: expected evaluation is 0.0f.
    // [1]: (After, before) states from real games: expected evaluation is 1.0f.
    std::vector<std::vector<REAL>> pre_paired_training_set[2];

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
            // Starting at the beginning of the game (when the position is
            // basically a tie) won't help us much, so let's arbitrarily
            // start halfway through the game.
            int starting_turn = h.hidx / 2;
            for (int i = 0; i < h.hidx; ++i) {
                const GameState& st = h.hvec[i].st;
                const GameState& newst = h.hvec[i+1].st;
                int who_just_moved = 1 - (i % 2); // hvec[1] is the state at the end of player 0's first move
                // This end-of-turn state is desirable for "winner" (if "winner" just moved)
                // or undesirable for "loser" (if "loser" just moved).
                if (i >= starting_turn) {
                    mix_and_match_training_set[who_just_moved != winner].push_back(encoder->encode(newst, who_just_moved));
                    auto worse_for_loser = encoder->encode(st, 1-who_just_moved);
                    auto better_for_winner = encoder->encode(newst, who_just_moved);
                    pre_paired_training_set[0].push_back(vec_concat(better_for_winner, worse_for_loser));
                    pre_paired_training_set[1].push_back(vec_concat(worse_for_loser, better_for_winner));
                }

                // We'll assume that the "winner" always improves his position
                // (relative to a move of "pass"). Also, let's assume the first
                // three moves are always improvements even for the loser.
                if (who_just_moved == winner || i < 6) {
                    auto worse_after_pass = encoder->encode(st, who_just_moved);
                    auto better_after_move = encoder->encode(newst, who_just_moved);
                    pre_paired_training_set[0].push_back(vec_concat(better_after_move, worse_after_pass));
                    pre_paired_training_set[1].push_back(vec_concat(worse_after_pass, better_after_move));
                }
            }
        }
    }

    void train() {
        auto& winners = mix_and_match_training_set[0];
        auto& losers = mix_and_match_training_set[1];

        std::mt19937 g;
        printf(
            "Training the evaluator on a set of %zu*%zu mix-match inputs and %zu pre-paired inputs...\n",
            winners.size(), losers.size(), pre_paired_training_set[0].size()
        );

        const int mmn = std::min(winners.size(), losers.size());
        const int ppn = pre_paired_training_set[0].size();
        assert(pre_paired_training_set[1].size() == size_t(ppn));
        for (int round = 0; round < 100; ++round) {
            std::shuffle(winners.begin(), winners.end(), g);
            std::shuffle(losers.begin(), losers.end(), g);
            printf("...Round %d\n", round);
            int successes = 0;
            for (int i=0; i < mmn; ++i) {
                successes += train_single_position(vec_concat(winners[i], losers[i]), 0.0f);
                successes += train_single_position(vec_concat(losers[i], winners[i]), 1.0f);
            }
            for (int i=0; i < ppn; ++i) {
                successes += train_single_position(pre_paired_training_set[0][i], 0.0f);
                successes += train_single_position(pre_paired_training_set[1][i], 1.0f);
            }
            printf("Success rate is %d/%d (%.2f%% correct)\n", successes, 2*(mmn + ppn), 50. * successes / (mmn + ppn));
        }
    }

    bool train_single_position(const std::vector<REAL>& in, REAL expected) {
        net.feedForward(in);
        std::vector<REAL> out = net.getResults();
        net.backProp(std::vector<REAL>(1, expected));
        return (out[0] > 0.5) == (expected > 0.5);
    }

    void save_to_file(FILE *fp) const {
        net.saveLayerToFile(0, fp);
        net.saveLayerToFile(1, fp);
        net.saveLayerToFile(2, fp);
    }
};
