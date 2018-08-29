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


struct AutoEncoderStage1 {
    // States we have seen in real games.
    std::set<State2VecVector> training_set;

    Net net{{900, 600, 900}};

    explicit AutoEncoderStage1() = default;

    void add_history_to_training_set(const History& h) {
        for (int i=0; i < h.hidx; ++i) {
            const GameState& st = h.hvec[i+1].st;
            training_set.insert(State2VecVector(st));
            training_set.insert(State2VecVector(st.mirror()));
        }
    }

    void train() {
        std::vector<State2VecVector> training_set_flat(training_set.begin(), training_set.end());
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);

        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<double> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                std::vector<double> s1_out;  // unused
                this->net.feedForward(s1_in);
                this->net.getResults(s1_out);
                printf("O1: %s\n", vec_to_string(s1_out).c_str());
            }
        }


        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        int count = 0;
        int report = 16;
        for (auto&& v : training_set_flat) {
            std::vector<double> s1_in = v.to_vector();
            net.feedForward(s1_in);
            std::vector<double> s1_out;
            net.getResults(s1_out);
            net.backProp(s1_in);
            if (++count == report) {
                report <<= 1;
                printf("%d...\n", count);
            }
        }


        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<double> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                std::vector<double> s1_out;  // unused
                this->net.feedForward(s1_in);
                this->net.getResults(s1_out);
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
        std::vector<State2VecVector> training_set_flat(stage1->training_set.begin(), stage1->training_set.end());
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);
        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        int count = 0;
        int report = 16;
        for (auto&& v : training_set_flat) {
            std::vector<double> s1_in = v.to_vector();
            net.feedForward(s1_in);
            std::vector<double> s1_out;
            net.getResults(s1_out);
            net.backProp(s1_in);
            if (++count == report) {
                report <<= 1;
                printf("%d...\n", count);
            }
        }

        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<double> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                std::vector<double> s1_out;  // unused
                this->net.feedForward(s1_in);
                this->net.getResults(s1_out);
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
        std::vector<State2VecVector> training_set_flat(stage1->training_set.begin(), stage1->training_set.end());
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);
        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        int count = 0;
        int report = 16;
        for (auto&& v : training_set_flat) {
            std::vector<double> s1_in = v.to_vector();
            net.feedForward(s1_in);
            std::vector<double> s1_out;
            net.getResults(s1_out);
            net.backProp(s1_in);
            if (++count == report) {
                report <<= 1;
                printf("%d...\n", count);
            }
        }

        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                std::vector<double> s1_in = v.to_vector();
                printf("I1: %s\n", vec_to_string(s1_in).c_str());
                std::vector<double> s1_out;  // unused
                this->net.feedForward(s1_in);
                this->net.getResults(s1_out);
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
