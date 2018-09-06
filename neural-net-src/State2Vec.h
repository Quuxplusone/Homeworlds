#pragma once

#include <assert.h>
#include <string>
#include <string.h>
#include "global.h"
#include "state.h"
#include "MillerNet.h"


struct State2VecVector {
    int8_t data[900];

    explicit State2VecVector(const GameState& st, int who_just_moved);

    friend bool operator< (const State2VecVector& a, const State2VecVector& b) {
        return memcmp(a.data, b.data, 900) < 0;
    }
    friend bool operator== (const State2VecVector& a, const State2VecVector& b) {
        return memcmp(a.data, b.data, 900) == 0;
    }

    std::vector<REAL> to_vector() const {
        return std::vector<REAL>(data, data+900);
    }

    const int8_t *begin() const { return data; }
    const int8_t *end() const { return data+900; }

    std::string to_string() const {
        std::string s;
        const char alphabet[65] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        for (int i=0; i < 900; i += 6) {
            int val = data[i]*32 + data[i+1]*16 + data[i+2]*8 + data[i+3]*4 + data[i+4]*2 + data[i+5]*1;
            assert(0 <= val && val <= 63);
            s += alphabet[val];
        }
        return s;
    }

    static State2VecVector from_string(const char *s) {
        State2VecVector vec;
        const char alphabet[65] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        for (int i=0; i < 900; i += 6) {
            char ch = *s++;
            const int val = strchr(alphabet, ch) - alphabet;
            vec.data[i] = (val & 0x20) != 0;
            vec.data[i+1] = (val & 0x10) != 0;
            vec.data[i+2] = (val & 0x8) != 0;
            vec.data[i+3] = (val & 0x4) != 0;
            vec.data[i+4] = (val & 0x2) != 0;
            vec.data[i+5] = (val & 0x1) != 0;
        }
        return vec;
    }

    void save_to_file(FILE *fp) const {
        fprintf(fp, "%s\n", this->to_string().c_str());
    }

    static State2VecVector from_file(FILE *fp) {
        char buf[900 / 2];  // oversized
        fscanf(fp, "%s", buf);
        return State2VecVector::from_string(buf);
    }

private:
    explicit State2VecVector() = default;
    void serialize(int8_t *first, const PieceCollection& pc, int depth);
};

inline State2VecVector::State2VecVector(const GameState& st, int who_just_moved) : data{}
{
    // Represent each star by 12 or 24 bits.
    // Represent each set of ships by 36 bits.
    // List the stash first.
    // Then list the two homeworlds.
    // Then list the other worlds in some arbitrary order.
    // Support up to 8 other worlds (84 bits each).
    // Normalize it so that the ships and homeworld of "who_just_moved" are listed first.

    int p0 = who_just_moved;
    int p1 = 1-who_just_moved;

    serialize(data + 0, st.stash, 36);
    if (const StarSystem *hw0 = st.homeworldOf(p0)) {
        serialize(data + 36, hw0->star, 24);
        serialize(data + 60, hw0->ships[p0], 36);
        serialize(data + 96, hw0->ships[p1], 36);
    }
    if (const StarSystem *hw1 = st.homeworldOf(p1)) {
        serialize(data + 132, hw1->star, 24);
        serialize(data + 156, hw1->ships[p0], 36);
        serialize(data + 192, hw1->ships[p1], 36);
    }
    int8_t *ptr = data + 228;
    for (const StarSystem& ss : st.stars) {
        if (ss.homeworldOf == -1) {
            serialize(ptr, ss.star, 12);
            serialize(ptr + 12, ss.ships[p0], 36);
            serialize(ptr + 48, ss.ships[p1], 36);
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
