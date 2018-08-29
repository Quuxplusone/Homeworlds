#pragma once

#include <memory>
#include <random>
#include <set>
#include <stdio.h>
#include <string>
#include <vector>
#include "History.h"
#include "State2Vec.h"

struct AutoEncoderStage1 {
    // States we have seen in real games.
    std::set<State2VecVector> training_set;

    using NetType = Net<900, 600, 900>;
    std::unique_ptr<NetType> net;

    AutoEncoderStage1() {
        this->net = std::make_unique<NetType>();
    }

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
                Vector<900> s1_in = v.to_vector();
                printf("I1: %s\n", s1_in.to_string().c_str());
                Vector<900> s1_out;  // unused
                this->net->feed_forward(s1_in, s1_out);
                printf("O1: %s\n", s1_out.to_string().c_str());
            }
        }


        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        net->set_learning_rate(0.005f);
        int round_size = training_set_flat.size() / 10;
        int count = 0;
        for (auto&& v : training_set_flat) {
            if (++count == round_size) {
                net->scale_learning_rate(0.98f);
                count = 0;
            }
            Vector<900> in = v.to_vector();
            Vector<900> out;
            net->feed_forward(in, out);
            Vector<900> out_error = (out - in);
            Vector<900> in_gradient;  // unused
            net->backpropagate_error(in_gradient, in, out, out_error);
        }


        // Demonstrate the results.
        if (true) {
            int count = 0;
            for (auto&& v : training_set_flat) {
                if (++count == 4) {
                    break;
                }
                Vector<900> s1_in = v.to_vector();
                printf("I1: %s\n", s1_in.to_string().c_str());
                Vector<900> s1_out;  // unused
                this->net->feed_forward(s1_in, s1_out);
                printf("O1: %s\n", s1_out.to_string().c_str());
            }
        }

    }
};

struct AutoEncoderStage2 {
    // States we have seen in real games.
    std::set<State2VecVector> training_set;

    using NetType = Net<600, 300, 600>;
    std::unique_ptr<NetType> net;
    AutoEncoderStage1 *stage1 = nullptr;

    AutoEncoderStage2(AutoEncoderStage1 *s1) {
        this->net = std::make_unique<NetType>();
        this->stage1 = s1;
    }

    void train() {
        std::vector<State2VecVector> training_set_flat(stage1->training_set.begin(), stage1->training_set.end());
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);
        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        net->set_learning_rate(0.005f);
        int round_size = training_set_flat.size() / 10;
        int count = 0;
        for (auto&& v : training_set_flat) {
            if (++count == round_size) {
                net->scale_learning_rate(0.98f);
                count = 0;
            }
            Vector<900> s1_in = v.to_vector();
            Vector<900> s1_out;  // unused
            stage1->net->feed_forward(s1_in, s1_out);
            const Vector<600>& s2_in = stage1->net->hidden;
            Vector<600> s2_out;
            this->net->feed_forward(s2_in, s2_out);
            Vector<600> out_error = (s2_out - s2_in);
            Vector<600> in_gradient;  // unused
            this->net->backpropagate_error(in_gradient, s2_in, s2_out, out_error);
        }
    }
};


struct AutoEncoderStage3 {
    // States we have seen in real games.
    std::set<State2VecVector> training_set;

    using NetType = Net<300, 100, 300>;
    std::unique_ptr<NetType> net;
    AutoEncoderStage2 *stage2 = nullptr;
    AutoEncoderStage1 *stage1 = nullptr;

    AutoEncoderStage3(AutoEncoderStage2 *s2) {
        this->net = std::make_unique<NetType>();
        this->stage2 = s2;
        this->stage1 = s2->stage1;
    }

    void train() {
        std::vector<State2VecVector> training_set_flat(stage1->training_set.begin(), stage1->training_set.end());
        std::mt19937 g;
        std::shuffle(training_set_flat.begin(), training_set_flat.end(), g);
        printf("Training on a set of %zu inputs...\n", training_set_flat.size());
        net->set_learning_rate(0.005f);
        int round_size = training_set_flat.size() / 10;
        int count = 0;
        for (auto&& v : training_set_flat) {
            if (++count == round_size) {
                net->scale_learning_rate(0.98f);
                count = 0;
            }
            Vector<900> s1_in = v.to_vector();
            Vector<900> s1_out;  // unused
            stage1->net->feed_forward(s1_in, s1_out);
            const Vector<600>& s2_in = stage1->net->hidden;
            Vector<600> s2_out;  // unused
            stage2->net->feed_forward(s2_in, s2_out);
            const Vector<300>& s3_in = stage2->net->hidden;
            Vector<300> s3_out;
            this->net->feed_forward(s3_in, s3_out);
            Vector<300> out_error = (s3_out - s3_in);
            Vector<300> in_gradient;  // unused
            this->net->backpropagate_error(in_gradient, s3_in, s3_out, out_error);
        }

        // Demonstrate the results.
        count = 0;
        for (auto&& v : training_set_flat) {
            if (++count == 4) {
                break;
            }
            Vector<900> s1_in = v.to_vector();
            printf("I1: %s\n", s1_in.to_string().c_str());
            Vector<900> s1_out;  // unused
            stage1->net->feed_forward(s1_in, s1_out);
            printf("O1: %s\n", s1_out.to_string().c_str());
            const Vector<600>& s2_in = stage1->net->hidden;
            printf("I2: %s\n", s2_in.to_string().c_str());
            Vector<600> s2_out;  // unused
            stage2->net->feed_forward(s2_in, s2_out);
            printf("O2: %s\n", s2_out.to_string().c_str());
            const Vector<300>& s3_in = stage2->net->hidden;
            printf("I3: %s\n", s3_in.to_string().c_str());
            Vector<300> s3_out;
            this->net->feed_forward(s3_in, s3_out);
            printf("O3: %s\n", s3_out.to_string().c_str());
        }
    }

    void save_to_file(FILE *fp) const {
        stage1->net->layer1.save_to_file(fp);
        stage2->net->layer1.save_to_file(fp);
        this->net->layer1.save_to_file(fp);
    }
};
