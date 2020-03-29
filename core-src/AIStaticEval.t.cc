
#include "AI.h"
#include "ApplyMove.h"
#include "WholeMove.h"
#include <gtest/gtest.h>

// GTest doesn't support XFAILing tests, so let's just do this for now.
#define XFAIL_GT(a, b) EXPECT_LE(a, b)
#define XFAIL_LT(a, b) EXPECT_GE(a, b)

static GameState from(const char *text)
{
    GameState st;
    EXPECT_TRUE(st.scan(text));
    return st;
}

static GameState apply(GameState st, const char *mtext)
{
    WholeMove m;
    EXPECT_TRUE(m.scan(mtext));
    EXPECT_EQ(ApplyMove::Whole(st, 0, m), ApplyMove::Result::SUCCESS);
    return st;
}

static GameState apply1(GameState st, const char *mtext)
{
    WholeMove m;
    EXPECT_TRUE(m.scan(mtext));
    EXPECT_EQ(ApplyMove::Whole(st, 1, m), ApplyMove::Result::SUCCESS);
    return st;
}

TEST(AIStaticEval, checkmate_opponent) {
    GameState st = from(R"(
        Player1 (0, r2g3) b1b3-
        Player2 (1, y3) r3b3b3-r1g1g1b1
        Delos (b2) -y2g1
        Terpsichore (y1) b2-
        Urania (y1) b1b2-
    )");
    GameState bad = apply(st, "sacrifice b2 at Urania; convert b3 to g3 at Player2; convert b3 to y3 at Player1");
    GameState good = apply(st, "sacrifice r3 at Player2; capture r1 at Player2; capture g1 at Player2; capture b1 at Player2");
    EXPECT_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, keep_opponent_in_check) {
    GameState st = from(R"(
        Player1 (0, r2g3) b1b3-
        Player2 (1, y3) r1b3b3-g1b1
        Delos (b2) -r1y2
        Terpsichore (y1) b2-
        Urania (y1) b1b2-
        Lebling (y3) -g1
    )");
    GameState bad = apply(st, "sacrifice b2 at Urania; convert b3 to g3 at Player2; convert b1 to y1 at Player1");
    GameState good = apply(st, "convert b3 to r3 at Player2");
    XFAIL_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, possible_catastrophe_of_only_big_ship_at_home) {
    GameState st = from(R"(
        Player1 (0, y1b2) g1r2g3-
        Player2 (1, y2b3) -g3y1g2y2
        Delos (b3) -b1b1g2y3g2r1
        g1 (g1) -y2y3
        g3 (g3) -y1y3
        Lebling (b3) r2r2b1r1g1-
    )");
    GameState bad = apply(st, "sacrifice g3 at Player1; build r1 at Player1; build r3 at Player1; build g3 at Lebling");
    GameState good = apply(st, "sacrifice g3 at Player1; build r1 at Player1; build r3 at Player1; build g3 at Player1");
    EXPECT_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, preventable_catastrophe) {
    GameState st = from(R"(
        Player1 (0, y2b3) r3g1-
        Player2 (1, r1b2) -y1g2g3
        Delos (b1) r1r2y1g1g1-
        Water (b3) -r1y2g2
    )");
    GameState bad = apply(st, "build r2 at Delos");
    GameState good = apply(st, "move r1 from Delos to Lebling (b3)");
    XFAIL_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, stash_management_green) {
    GameState st = from(R"(
        Player1 (0, y2b3) g1g3-
        Player2 (1, g1b2) -g3y1
        Alpha (b1) g1g2-
        Beta (g3) -y1
    )");
    GameState bad = apply(st, "sacrifice g3 at Player1; build g2 at Alpha; build g2g3 at Player1");
    GameState good = apply(st, "convert g1 to r1 at Alpha");
    EXPECT_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, threatened_capture_at_home) {
    GameState st = from(R"(
        Player1 (0, y3g2) r1b3-y1y2g1
        Player2 (1, r1b2) -r2y1y3
        Betazed (b3) y1y2y3g1g3-
        Bolarus (b3) -g1
        Lebling (r1) -y2g2
    )");
    GameState bad = apply(st, "sacrifice y3 at Betazed; move y1 from Betazed to Player2; move y2 from Betazed to Player2; move g3 from Betazed to Lebling; catastrophe yellow at Player2");
    GameState good = apply(st, "sacrifice y3 at Betazed; move y1 from Betazed to Player2; move y2 from Betazed to Player2; move g3 from Betazed to Player2; catastrophe yellow at Player2");
    EXPECT_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, capture_to_win) {
    GameState st = from(R"(
        Player1 (0, r1b2) g1g3b1-
        Player2 (1, y3b2) r3-r2g1g2
        Delos (b1) r1-
        Kotok (b3) -y1g1
    )");
    GameState bad = apply(st, "trade g1 y1 Player1");
    GameState good = apply(st, "attack r2 Player2");
    XFAIL_GT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}

TEST(AIStaticEval, fumbled_setup) {
    GameState good = from(R"(
        Player1 (0, y1b2) g3-
        Player2 (1, r1b2) -g3
    )");
    GameState bad = from(R"(
        Player1 (0, y1b2) g3-
        Player2 (1, y1b2) -g3
    )");
    XFAIL_GT(ai_static_evaluation(good, 1), ai_static_evaluation(bad, 1));
    good = apply(good, "build g1 Player1");
    bad = apply(bad, "build g1 Player1");
    XFAIL_LT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
    good = apply1(good, "build g1 Player2");
    bad = apply1(bad, "build g1 Player2");
    XFAIL_GT(ai_static_evaluation(good, 1), ai_static_evaluation(bad, 1));
    good = apply(good, "trade g1 y1 Player1");
    bad = apply(bad, "trade g1 y1 Player1");
    XFAIL_LT(ai_static_evaluation(good, 0), ai_static_evaluation(bad, 0));
}
