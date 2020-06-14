
#include "ApplyMove.h"
#include "Retrograde.h"
#include "WholeMove.h"
#include <gtest/gtest.h>

static bool can_reach(GameState *targetst, const GameState& newst, WholeMove *foundm)
{
    std::string target = targetst->toComparableString();
    return findAllRetrograde(
        newst, 0,
        [&](const WholeMove& m, const GameState& oldst) {
            if (oldst.toComparableString() == target) {
                // Since "oldst" may contain star systems that are no longer present
                // in "newst", and those names may be used in "m", we must update
                // both "targetst" and "foundm" with consistent names.
                *foundm = m;
                *targetst = oldst;
                return true;
            }
            return false;
        }
    );
}

#define EXPECT_PREDECESSOR(newst, oldst_text) do { \
        GameState oldst = from(oldst_text); \
        WholeMove m; \
        EXPECT_TRUE(can_reach(&oldst, newst, &m)); \
        EXPECT_TRUE(m.sanitycheck()); \
        EXPECT_EQ(ApplyMove::Whole(oldst, 0, m), ApplyMove::Result::SUCCESS); \
        EXPECT_EQ(oldst.toComparableString(), newst.toComparableString()); \
    } while (0)

#define EXPECT_NO_PREDECESSOR(newst, oldst_text) do { \
        GameState oldst = from(oldst_text); \
        WholeMove m; \
        EXPECT_FALSE(can_reach(&oldst, newst, &m)); \
    } while (0)

#define XFAIL_PREDECESSOR(a, b) EXPECT_NO_PREDECESSOR(a, b)

static GameState from(const char *text)
{
    GameState st;
    EXPECT_TRUE(st.scan(text));
    return st;
}

TEST(Retrograde, capture_simple) {
    GameState st = from(R"(
        Player1 (0, r3) g3g1-
        Player2 (1, r3) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) g3-g1
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g1-g3
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_capture_of_bigger_ship) {
    GameState st = from(R"(
        Player1 (0, g3) r1y2g3-
        DS1 (g3) r2-
        Player2 (1, r3) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) r1g3-y2
        DS1 (g3) r2-
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) y2g3-r1
        DS1 (g3) r2-
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) r1y2g3-
        DS1 (g3) -r2
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_capture_without_red_access) {
    GameState st = from(R"(
        Player1 (0, g3) y2g3-
        DS1 (g3) b3-
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-y2
        DS1 (g3) b3-
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) g3y2-
        DS1 (g3) -b3
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, move_simple) {
    GameState st = from(R"(
        Player1 (0, g3) y2g3-
        DS1 (g2) b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS1 (g2) y2b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        Player2 (1, y1) y2-b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        Player2 (1, y1) g3-b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) y2-
        DS1 (g2) g3b3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, discover_simple) {
    GameState st = from(R"(
        Player1 (0, g3) y2g3-
        DS1 (g2) b3-
        DS2 (r1) g1-
        DS3 (y2) y1-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) y2g3b3-
        DS2 (r1) g1-
        DS3 (y2) y1-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) y2g3-
        DS2 (r1) g1-
        DS3 (y2) y1-
        Player2 (1, y1) b3-b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) y2g3-
        DS2 (r1) g1b3-
        DS3 (y2) y1-
        Player2 (1, y1) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) y2g3-
        DS2 (r1) g1-
        DS3 (y2) y1b3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, move_with_dismantling) {
    GameState st = from(R"(
        Player1 (0, g3) y2g3-
        DS1 (g2) b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        DS2 (g2) y2-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        DS2 (y1) y2-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        DS2 (y1) g3-
        Player2 (1, y1) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        DS2 (y3) y2-
        Player2 (1, y1) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        DS2 (y3) g3-
        Player2 (1, y1) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        DS2 (b1) g3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, discover_with_dismantling) {
    GameState st = from(R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS2 (y1) b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS2 (y3) b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS2 (y2) b3-
        Player2 (1, y1) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) g3-
        DS2 (b3) b3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, build_simple) {
    GameState st = from(R"(
        Player1 (0, r3) g2g1b2b1y1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) g2b2b1y1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) g2g1b1y1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) g2g1b2y1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g1b2b1y1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g2g1b2b1-
        Player2 (1, r3b1) -b1
    )");
}

TEST(Retrograde, impossible_build_without_green_access) {
    GameState st = from(R"(
        Player1 (0, r3) b2b1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) b2-
        Player2 (1, r3b1) -b1
    )");
}

TEST(Retrograde, trade_away_only_blue) {
    GameState st = from(R"(
        Player1 (0, g3) g1-
        Player2 (1, r3) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) b1-
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) r1-
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_trade_away_from_overpopulation) {
    GameState st = from(R"(
        Player1 (0, g3) b1b1-g1g1
        Player2 (1, r3) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) b1r1-g1g1
        Player2 (1, r3) -b3
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, g3) b1y1-g1g1
        Player2 (1, r3) -b3
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, g3) b1g1-g1g1
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_trade_without_blue_access) {
    GameState st = from(R"(
        Player1 (0, r3) g2g1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) y2g1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) b2g1-
        Player2 (1, r3b1) -b1
    )");
}

// TODO FIXME BUG HACK: What about moves that simply catastrophe part of the state?
TEST(Retrograde, free_move_to_catastrophe) {
    GameState st = from(R"(
        Player1 (0, r3) g2-
        Player2 (1, r3b1) -b1
    )");
    XFAIL_PREDECESSOR(st, R"(
        Player1 (0, r3) g2y1-
        Player1 (y1) y2-y1
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g2y1-
        Player1 (y3) y2-y1
        Player2 (1, r3b1) -b1
    )");
    XFAIL_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (y1) y2-y1
        Player1 (y3) y2-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (y1) y2-y1
        Player1 (y1) y2-
        Player2 (1, r3b1) -b1
    )");
}

// TODO FIXME BUG HACK: What about moves that simply catastrophe part of the state?
TEST(Retrograde, free_build_to_catastrophe) {
    GameState st = from(R"(
        Player1 (0, r3) g2-
        Player2 (1, r3b1) -b1
    )");
    XFAIL_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (b1) g1b2-b2
        Player2 (1, r3b1) -b1
    )");
    XFAIL_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (g1) b2-b2b2
        Player2 (1, r3b1) -b1
    )");
}

// TODO FIXME BUG HACK: What about moves that simply catastrophe part of the state?
TEST(Retrograde, free_trade_to_catastrophe) {
    GameState st = from(R"(
        Player1 (0, r3) g2-
        Player2 (1, r3b1) -b1
    )");
    XFAIL_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (y1) r3b1-y2y2y2
        Player2 (1, r3b1) -b1
    )");
    XFAIL_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (r1) b1y2-y2y2
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (r1) b2y2-y2y2
        Player2 (1, r3b1) -b1
    )");
}

TEST(Retrograde, pointless_sacrifice) {
    GameState st = from(R"(
        Player1 (0, r3) g2-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (y1) r3-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (b3) b1-
        Player2 (1, r3b1) -b1
    )");
    EXPECT_NO_PREDECESSOR(st, R"(
        Player1 (0, r3) g2-
        Player1 (b3) r3r1-
        Player2 (1, r3b1) -b1
    )");
}


