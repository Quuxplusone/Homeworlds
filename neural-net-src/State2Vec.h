#pragma once

#include <assert.h>
#include <string>
#include <string.h>
#include "global.h"
#include "state.h"
#include "MillerNet.h"


struct State2VecVector {
    int8_t data[900];

    explicit State2VecVector(const GameState&);

    friend bool operator< (const State2VecVector& a, const State2VecVector& b) {
        return memcmp(a.data, b.data, 900) < 0;
    }
    friend bool operator== (const State2VecVector& a, const State2VecVector& b) {
        return memcmp(a.data, b.data, 900) == 0;
    }

    std::vector<double> to_vector() const {
        return std::vector<double>(data, data+900);
    }

    std::string to_string() const {
        std::string s;
        char alphabet[65] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        for (int i=0; i < 900; i += 6) {
            int val = data[i]*32 + data[i+1]*16 + data[i+2]*8 + data[i+3]*4 + data[i+4]*2 + data[i+5]*1;
            assert(0 <= val && val <= 63);
            s += alphabet[val];
        }
        return s;
    }

private:
    void serialize(int8_t *first, const PieceCollection& pc, int depth);
};

inline State2VecVector::State2VecVector(const GameState& st) : data{}
{
    // Assume that the attacker is player 0, and the defender is player 1.

    // Represent each star by 12 or 24 bits.
    // Represent each set of ships by 36 bits.
    // List the stash first.
    // Then list the two homeworlds.
    // Then list the other worlds in some arbitrary order.
    // Support up to 8 other worlds (84 bits each).

    serialize(data + 0, st.stash, 36);
    if (const StarSystem *hw0 = st.homeworldOf(0)) {
        serialize(data + 36, hw0->star, 24);
        serialize(data + 60, hw0->ships[0], 36);
        serialize(data + 96, hw0->ships[1], 36);
    }
    if (const StarSystem *hw1 = st.homeworldOf(1)) {
        serialize(data + 132, hw1->star, 24);
        serialize(data + 156, hw1->ships[0], 36);
        serialize(data + 192, hw1->ships[1], 36);
    }
    int8_t *ptr = data + 228;
    for (const StarSystem& ss : st.stars) {
        if (ss.homeworldOf == -1) {
            serialize(ptr, ss.star, 12);
            serialize(ptr + 12, ss.ships[0], 36);
            serialize(ptr + 48, ss.ships[1], 36);
            ptr += 84;
            if (ptr == data + 900) {
                break;
            }
        }
    }
}

inline void State2VecVector::serialize(int8_t *first, const PieceCollection& pc, int depth)
{
    depth /= 12;
    int8_t (*ptr)[4][3] = reinterpret_cast<int8_t (*)[4][3]>(first);
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            int n = pc.numberOf(c, s);
            assert(n <= depth);
            for (int i=0; i < n; ++i) {
                ptr[i][c][s] = 1;
            }
        }
    }
}
