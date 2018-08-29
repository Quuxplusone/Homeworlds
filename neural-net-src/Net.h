#pragma once

#include <array>
#include <math.h>
#include <stdlib.h>

template<int N>
class Vector {
    float data_[N] {};
public:
    float& operator[](int i) { return data_[i]; }
    const float& operator[](int i) const { return data_[i]; }

    void clear() {
        for (float& f : data_) {
            f = 0.0f;
        }
    }

    friend Vector operator- (const Vector& a, const Vector& b) {
        Vector result;
        for (int i=0; i < N; ++i) {
            result[i] = a[i] - b[i];
        }
        return result;
    }

    friend Vector operator- (const Vector& a, float b) {
        Vector result;
        for (int i=0; i < N; ++i) {
            result[i] = a[i] - b;
        }
        return result;
    }

    template<int M>
    Vector<N+M> concat(const Vector<M>& b) {
        Vector<N+M> result;
        for (int i=0; i < N; ++i) {
            result[i] = (*this)[i];
        }
        for (int i=0; i < M; ++i) {
            result[N+i] = b[i];
        }
        return result;
    }

    std::string to_string() const {
        std::string result;
        for (float f : data_) {
            if (-0.5 < f && f < 0.5) result += '0';
            else if (f < 0) result += '-';
            else if (f < 1.5) result += '1';
            else result += '+';
        }
        return result;
    }
};

template<int N0, int N1>
struct WeightsLayer {
    float weights[N0 + 1][N1];
    float learning_rate = 0.005;

    explicit WeightsLayer() {
        for (auto& array : weights) {
            for (float& f : array) {
                f = (rand() / float(RAND_MAX)) - 0.5f;
            }
        }
    }

    static float sigmoid(float x) {
        return 1.0f / (1.0f + expf(-x));
    }

    static float sigmoid_gradient(float sx) {
        return sx * (1.0f - sx);
    }

    static float or_bias(const Vector<N0>& in, int i) {
        if (i == N0) {
            return 1.0f;
        } else {
            return in[i];
        }
    }

    void feed_forward(const Vector<N0>& in, Vector<N1>& out) const {
        for (int j=0; j < N1; ++j) {
            float val = 0.0f;
            for (int i=0; i < N0 + 1; ++i) {
                val += or_bias(in, i) * weights[i][j];
            }
            out[j] = sigmoid(val);
        }
    }

    void backpropagate_error(Vector<N0>& in_gradient, const Vector<N0>& in, const Vector<N1>& out, const Vector<N1>& out_error) {

        // Compute "output gradients"
        Vector<N1> out_gradient;
        for (int j=0; j < N1; ++j) {
            out_gradient[j] = out_error[j] * sigmoid_gradient(out[j]);
        }

        backpropagate_gradient(in_gradient, in, out, out_gradient);
    }

    void backpropagate_gradient(Vector<N0>& in_gradient, const Vector<N0>& in, const Vector<N1>& out, const Vector<N1>& out_gradient) {

        for (int i=0; i < N0; ++i) {
            float dow = 0.0;
            for (int j=0; j < N1; ++j) {
                dow += weights[i][j] * out_gradient[j];
            }
            in_gradient[i] = dow * sigmoid_gradient(in[i]);
        }

        for (int j=0; j < N1; ++j) {
            for (int i=0; i < N0 + 1; ++i) {
                float delta = learning_rate * or_bias(in, i) * out_gradient[j];
                weights[i][j] -= delta;
            }
        }
    }

    void save_to_file(FILE *fp) const {
        for (const auto& array : weights) {
            for (float f : array) {
                fprintf(fp, " %.3f", f);
            }
            fprintf(fp, "\n");
        }
    }
    void load_from_file(FILE *fp) {
        for (auto& array : weights) {
            for (float& f : array) {
                fscanf(fp, "%f", &f);
            }
        }
    }
};

template<int N0, int N1, int N2>
struct Net {
    WeightsLayer<N0, N1> layer1;
    Vector<N1> hidden;
    WeightsLayer<N1, N2> layer2;

    void set_learning_rate(float r) {
        layer1.learning_rate = r;
        layer2.learning_rate = r;
    }

    void scale_learning_rate(float r) {
        layer1.learning_rate *= r;
        layer2.learning_rate *= r;
    }

    void feed_forward(const Vector<N0>& in, Vector<N2>& out) {
        layer1.feed_forward(in, hidden);
        layer2.feed_forward(hidden, out);
    }

    void backpropagate_error(Vector<N0>& in_gradient, const Vector<N0>& in, const Vector<N2>& out, const Vector<N2>& out_error) {
        Vector<N1> hidden_gradient;
        layer2.backpropagate_error(hidden_gradient, hidden, out, out_error);
        layer1.backpropagate_gradient(in_gradient, in, hidden, hidden_gradient);
    }
};
