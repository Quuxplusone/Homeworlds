#pragma once

#include <memory>
#include <random>
#include <set>
#include <stdio.h>
#include <string>
#include <vector>
#include "History.h"
#include "MillerNet.h"
#include "State2Vec.h"
#if 1
#include "AllMoves.h"
#include "ApplyMove.h"
#endif


struct AutoEncoderStage1 {
    // States we have seen in real games.
    std::set<State2VecVector> training_set;
    std::vector<State2VecVector> training_set_flat;

    Net net{{900, 600, 900}};

    explicit AutoEncoderStage1() = default;

    void add_history_to_training_set(const History& h) {
        for (int i=0; i <= h.hidx; ++i) {
            const GameState& st = h.hvec[i].st;
            training_set.insert(State2VecVector(st, 0));
            training_set.insert(State2VecVector(st, 1));
        }
#if 1
        // Also train on hypothetical positions that might be reached
        // FROM these real positions, since we will be called upon to
        // evaluate such positions during gameplay.
        for (int i=0; i < h.hidx; ++i) {
            const GameState& st = h.hvec[i].st;
            int attacker = (i % 2);
            const unsigned int all_colors_but_yellow =
                ((1u << RED) | (1u << GREEN) | (1u << BLUE));
            std::vector<WholeMove> allmoves;

            findAllMoves(st, attacker, allmoves, /*prune_obviously_worse_moves=*/false,
                /*look_only_for_wins=*/false, all_colors_but_yellow);
            for (const auto& move : allmoves) {
                GameState newst = st;
                ApplyMove::or_die(newst, attacker, move);
                training_set.insert(State2VecVector(newst, 0));
                training_set.insert(State2VecVector(newst, 1));
            }
        }
#endif
    }

    void save_training_set_to_file(FILE *fp) const {
        fprintf(fp, "%zu\n", training_set.size());
        for (auto&& vec : training_set) {
            vec.save_to_file(fp);
        }
    }

    void load_training_set_from_file(FILE *fp) {
        size_t n = 0;
        fscanf(fp, "%zu", &n);
        training_set_flat.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            training_set_flat.push_back(State2VecVector::from_file(fp));
        }
    }

    void train() {
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);

        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<REAL> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                this->net.feedForward(s1_in);
                std::vector<REAL> s1_out = this->net.getResults();
                printf("O1: %s\n", vec_to_string(s1_out).c_str());
            }
        }


        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        int count = 0;
        int report = 16;
        for (auto&& v : training_set_flat) {
            std::vector<REAL> s1_in = v.to_vector();
            net.feedForward(s1_in);
            net.backProp(s1_in);
            if (++count == report) {
                report <<= 1;
                printf("%d...\n", count);
                if (count == 8192) break;
            }
        }


        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<REAL> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                this->net.feedForward(s1_in);
                std::vector<REAL> s1_out = this->net.getResults();
                printf("O1: %s\n", vec_to_string(s1_out).c_str());
            }
        }

    }
};

struct AutoEncoderStage2 {
    Net net{{900, 600, 300, 600, 900}};
    AutoEncoderStage1 *stage1 = nullptr;

    explicit AutoEncoderStage2(AutoEncoderStage1 *s1) {
        stage1 = s1;
        net.copyWeightsFrom(0, s1->net, 0);
        net.copyWeightsFrom(3, s1->net, 1);
    }

    void train() {
        auto& training_set_flat = stage1->training_set_flat;
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);
        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        int count = 0;
        int report = 16;
        for (auto&& v : training_set_flat) {
            std::vector<REAL> s1_in = v.to_vector();
            net.feedForward(s1_in);
            net.backProp(s1_in);
            if (++count == report) {
                report <<= 1;
                printf("%d...\n", count);
                if (count == 8192) break;
            }
        }

        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<REAL> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                this->net.feedForward(s1_in);
                std::vector<REAL> s1_out = this->net.getResults();
                printf("O1: %s\n", vec_to_string(s1_out).c_str());
            }
        }

    }
};


struct AutoEncoderStage3 {
    Net net{{900, 600, 300, 100, 300, 600, 900}};
    AutoEncoderStage1 *stage1 = nullptr;

    explicit AutoEncoderStage3(AutoEncoderStage2 *s2) {
        stage1 = s2->stage1;
        net.copyWeightsFrom(0, s2->net, 0);
        net.copyWeightsFrom(1, s2->net, 1);
        net.copyWeightsFrom(4, s2->net, 2);
        net.copyWeightsFrom(5, s2->net, 3);
    }

    void train() {
        auto& training_set_flat = stage1->training_set_flat;
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);
        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        int count = 0;
        int report = 16;
        for (auto&& v : training_set_flat) {
            std::vector<REAL> s1_in = v.to_vector();
            net.feedForward(s1_in);
            net.backProp(s1_in);
            if (++count == report) {
                report <<= 1;
                printf("%d...\n", count);
                if (count == 8192) break;
            }
        }

        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<REAL> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                this->net.feedForward(s1_in);
                std::vector<REAL> s1_out = this->net.getResults();
                printf("O1: %s\n", vec_to_string(s1_out).c_str());
            }
        }

    }

    void save_to_file(FILE *fp) const {
        net.saveLayerToFile(0, fp);
        net.saveLayerToFile(1, fp);
        net.saveLayerToFile(2, fp);
    }
};
