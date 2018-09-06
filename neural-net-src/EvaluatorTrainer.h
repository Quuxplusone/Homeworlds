#pragma once

#include <memory>
#include <random>
#include <set>
#include <stdio.h>
#include <string>
#include <time.h>
#include <vector>
#include "AllMoves.h"
#include "GameStateEncoder.h"
#include "History.h"
#include "State2Vec.h"

struct EvaluatorTrainer {
    struct BeforeAfter {
        State2VecVector before;
        State2VecVector after;
        explicit BeforeAfter(State2VecVector&& b, State2VecVector&& a) : before(std::move(b)), after(std::move(a)) {}
    };
    struct EncodedBeforeAfter {
        std::vector<REAL> before;
        std::vector<REAL> after;
        explicit EncodedBeforeAfter(std::vector<REAL>&& b, std::vector<REAL>&& a) : before(std::move(b)), after(std::move(a)) {}
    };

    // States we have seen in real games (before and after p0's move).
    std::vector<BeforeAfter> before_and_after_training_set;

    // [0]: States we have seen in real games (after p0's move) where player 0 ended up winning.
    // [1]: States we have seen in real games (after p0's move) where player 1 ended up winning.
    std::vector<State2VecVector> mix_and_match_training_set[2];

    Net net{{200, 100, 50, 1}};
    GameStateEncoder *encoder;

    explicit EvaluatorTrainer(GameStateEncoder *e) {
        encoder = e;
    }

    static int identifyWinner(const GameState& st, int non_resigner) {
        if (st.hasLost(0)) {
            if (st.hasLost(1)) {
                return -1;
            } else {
                return 1;
            }
        } else if (st.hasLost(1)) {
            return 0;
        } else {
            WholeMove winmove;
            if (findWinningMove(st, non_resigner, &winmove)) {
                return non_resigner;
            }
        }

        return -1;
    }

    void maybe_add_history_to_training_set(const History& h) {
        int who_made_the_final_move_of_the_game = 1 - (h.hidx % 2);
        int winner = identifyWinner(h.currentstate(), who_made_the_final_move_of_the_game);
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
                    mix_and_match_training_set[who_just_moved != winner].push_back(State2VecVector(newst, who_just_moved));
                    if (who_just_moved == winner) {
                        before_and_after_training_set.emplace_back(
                            State2VecVector(st, 1-who_just_moved),
                            State2VecVector(newst, who_just_moved)
                        );
                    } else {
                        before_and_after_training_set.emplace_back(
                            State2VecVector(newst, who_just_moved),
                            State2VecVector(st, 1-who_just_moved)
                        );
                    }
                }

                // We'll assume that the "winner" always improves his position
                // (relative to a move of "pass"). Also, let's assume the first
                // three moves are always improvements even for the loser.
                if (who_just_moved == winner || i < 6) {
                    before_and_after_training_set.emplace_back(
                        State2VecVector(st, who_just_moved),
                        State2VecVector(newst, who_just_moved)
                    );
                }
            }
        }
    }

    void save_training_set_to_file(FILE *fp) const {
        printf(
            "Saving a training set of size (%zu, %zu, %zu)\n",
            mix_and_match_training_set[0].size(),
            mix_and_match_training_set[1].size(),
            before_and_after_training_set.size()
        );
        fprintf(fp, "%zu\n", mix_and_match_training_set[0].size());
        for (const auto& vec : mix_and_match_training_set[0]) {
            vec.save_to_file(fp);
        }
        fprintf(fp, "%zu\n", mix_and_match_training_set[1].size());
        for (const auto& vec : mix_and_match_training_set[1]) {
            vec.save_to_file(fp);
        }
        fprintf(fp, "%zu\n", before_and_after_training_set.size());
        for (const BeforeAfter& elt : before_and_after_training_set) {
            elt.before.save_to_file(fp);
            elt.after.save_to_file(fp);
        }
    }

    void load_training_set_from_file(FILE *fp) {
        size_t n = 0;
        fscanf(fp, "%zu", &n);
        mix_and_match_training_set[0].reserve(n);
        for (size_t i = 0; i < n; ++i) {
            mix_and_match_training_set[0].push_back(State2VecVector::from_file(fp));
        }
        fscanf(fp, "%zu", &n);
        mix_and_match_training_set[1].reserve(n);
        for (size_t i = 0; i < n; ++i) {
            mix_and_match_training_set[1].push_back(State2VecVector::from_file(fp));
        }
        fscanf(fp, "%zu", &n);
        before_and_after_training_set.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            auto before = State2VecVector::from_file(fp);
            auto after = State2VecVector::from_file(fp);
            before_and_after_training_set.emplace_back(std::move(before), std::move(after));
        }
    }

    void train() {
        printf("Pre-encoding the training set...\n");
        int sofar_encode = 0;
        int total_encode = mix_and_match_training_set[0].size() + mix_and_match_training_set[1].size() + 2*before_and_after_training_set.size();

        std::vector<std::vector<REAL>> winners;
        winners.reserve(mix_and_match_training_set[0].size());
        for (auto&& vec : mix_and_match_training_set[0]) {
            winners.push_back(encoder->encode(vec));
            sofar_encode += 1;
            if (sofar_encode % 256 == 0) {
                printf(".   %d/%d t=%zu\n", sofar_encode, total_encode, size_t(time(nullptr)));
            }
        }
        std::vector<std::vector<REAL>> losers;
        losers.reserve(mix_and_match_training_set[1].size());
        for (auto&& vec : mix_and_match_training_set[1]) {
            losers.push_back(encoder->encode(vec));
            sofar_encode += 1;
            if (sofar_encode % 256 == 0) {
                printf("..  %d/%d t=%zu\n", sofar_encode, total_encode, size_t(time(nullptr)));
            }
        }
        std::vector<EncodedBeforeAfter> encoded_before_after;
        encoded_before_after.reserve(before_and_after_training_set.size());
        for (auto&& elt : before_and_after_training_set) {
            encoded_before_after.emplace_back(
                encoder->encode(elt.before),
                encoder->encode(elt.after)
            );
            sofar_encode += 2;
            if (sofar_encode % 256 < 2) {
                printf("... %d/%d t=%zu\n", sofar_encode, total_encode, size_t(time(nullptr)));
            }
        }

        printf(
            "Training the evaluator on a set of %zu*%zu mix-match inputs and %zu before-and-after inputs...\n",
            winners.size(), losers.size(), encoded_before_after.size()
        );

        const int mmn = std::min(winners.size(), losers.size());
        const int ppn = before_and_after_training_set.size();

        std::mt19937 g;
        for (int round = 0; round < 100; ++round) {
            std::shuffle(winners.begin(), winners.end(), g);
            std::shuffle(losers.begin(), losers.end(), g);
            printf("...Round %d\n", round);
            int successes = 0;
            for (int i = 0; i < mmn; ++i) {
                successes += train_single_position(winners[i], losers[i], 0.0f);
                successes += train_single_position(losers[i], winners[i], 1.0f);
            }
            for (int i = 0; i < ppn; ++i) {
                auto&& elt = encoded_before_after[i];
                successes += train_single_position(elt.after, elt.before, 0.0f);
                successes += train_single_position(elt.before, elt.after, 1.0f);
            }
            printf("Success rate is %d/%d (%.2f%% correct)\n", successes, 2*(mmn + ppn), 50. * successes / (mmn + ppn));
        }
    }

    bool train_single_position(const std::vector<REAL>& in1, const std::vector<REAL>& in2, REAL expected) {
        net.feedForward(in1, in2);
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
