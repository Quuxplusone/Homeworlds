
#include "state.h"
#include <gtest/gtest.h>

TEST(GameState, ctor) {
    GameState st;
    EXPECT_TRUE(st.stash.empty());
    EXPECT_TRUE(st.stars.empty());
}

TEST(GameState, newGame) {
    GameState st;
    st.newGame();
    EXPECT_EQ(st.stash.toString(), "r1r1r1r2r2r2r3r3r3y1y1y1y2y2y2y3y3y3g1g1g1g2g2g2g3g3g3b1b1b1b2b2b2b3b3b3");
    EXPECT_TRUE(st.stars.empty());
}

TEST(GameState, scan) {
    GameState st;
    EXPECT_TRUE(st.scan(R"(
        Sam (0, g1) b2y1-
        (b2) r1-r1
        Dave (1, g2) -y3
    )"));
    EXPECT_EQ(st.toString(), "Sam (0, g1) y1b2-\n(b2) r1-r1\nDave (1, g2) -y3\n");
    EXPECT_NE(st.systemNamed("Sam"), nullptr);
    EXPECT_NE(st.systemNamed("Dave"), nullptr);
    EXPECT_EQ(st.systemNamed("Eric"), nullptr);
    EXPECT_EQ(st.homeworldOf(0), st.systemNamed("Sam"));
    EXPECT_EQ(st.homeworldOf(1), st.systemNamed("Dave"));
    EXPECT_FALSE(st.gameIsOver());
    EXPECT_FALSE(st.hasLost(0));
    EXPECT_FALSE(st.hasLost(1));
    EXPECT_EQ(st.stash.toString(), "r1r2r2r2r3r3r3y1y1y2y2y2y3y3g1g1g2g2g3g3g3b1b1b1b2b3b3b3");
}

TEST(GameState, homeworldOfMissingPlayer) {
    GameState st;
    EXPECT_TRUE(st.scan(R"(
        Sam (0, g1) b2y1-
        (b2) r1-r1
    )"));
    EXPECT_EQ(st.toString(), "Sam (0, g1) y1b2-\n(b2) r1-r1\n");
    EXPECT_NE(st.systemNamed("Sam"), nullptr);
    EXPECT_EQ(st.homeworldOf(0), st.systemNamed("Sam"));
    EXPECT_EQ(st.homeworldOf(1), nullptr);
    EXPECT_TRUE(st.gameIsOver());
    EXPECT_FALSE(st.hasLost(0));
    EXPECT_TRUE(st.hasLost(1));

    EXPECT_TRUE(st.scan(R"(
        Sam (1, g1) -y1b2
    )"));
    EXPECT_EQ(st.toString(), "Sam (1, g1) -y1b2\n");
    EXPECT_NE(st.systemNamed("Sam"), nullptr);
    EXPECT_EQ(st.homeworldOf(0), nullptr);
    EXPECT_EQ(st.homeworldOf(1), st.systemNamed("Sam"));
    EXPECT_TRUE(st.gameIsOver());
    EXPECT_TRUE(st.hasLost(0));
    EXPECT_FALSE(st.hasLost(1));
}
