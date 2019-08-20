#pragma once

#include "global.h"
#include <assert.h>
#include <stdio.h>
#include <string>
#include <vector>

#include "SingleAction.h"

class WholeMove {
public:
    bool isPass() const {
        return actions.empty();
    }

    /* We use this operation to build up whole moves piece by piece.
     * Note that any prefix of a legal move is itself a legal move,
     * so it's safe to sanity-check both the input and the output of
     * this operation. */
    WholeMove &operator += (SingleAction a) {
        assert(this->sanitycheck());
        assert(a.sanitycheck());
        actions.push_back(std::move(a));
        assert(this->sanitycheck());
        return *this;
    }

    bool is_missing_pieces() const;
    int unusedSacrificeActions() const;

    std::string toString() const;
    std::string toSDGString() const;
    bool scan(const char *text);
    explicit WholeMove() { assert(this->isPass()); }
    explicit WholeMove(const char *text) { const bool UNUSED(rc) = scan(text); assert(rc); }
    explicit WholeMove(const std::string &text) : WholeMove(text.c_str()) {}
    explicit WholeMove(const WholeMove &m, const SingleAction &a): actions(m.actions)
        { *this += a; }

    bool sanitycheck() const;

public:
    /* These fields should be treated as read-only. */
    std::vector<SingleAction> actions;
};
