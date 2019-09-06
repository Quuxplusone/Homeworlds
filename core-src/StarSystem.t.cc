
#include "state.h"
#include <gtest/gtest.h>

TEST(StarSystem, ctor) {
    StarSystem ss("Foobar");
    EXPECT_EQ(ss.name, "Foobar");
    EXPECT_TRUE(ss.pieceCollection().empty());
    EXPECT_EQ(ss.numberOfShips(), 0);
    EXPECT_TRUE(ss.hasNoShips());
}

TEST(StarSystem, accessors) {
    StarSystem ss; ss.scan("Foobar (0, b2g1) r1r2-y3b1");
    EXPECT_EQ(ss.name, "Foobar");
    EXPECT_EQ(ss.homeworldOf, 0);
    EXPECT_EQ(ss.pieceCollection().toString(), "r1r2y3g1b1b2");
    EXPECT_EQ(ss.numberOfShips(), 4);
    EXPECT_EQ(ss.numberOf(RED), 2);
    EXPECT_EQ(ss.numberOf(YELLOW), 1);
    EXPECT_EQ(ss.numberOf(GREEN), 1);
    EXPECT_EQ(ss.numberOf(BLUE), 2);
    EXPECT_FALSE(ss.containsOverpopulation());
}

TEST(StarSystem, homeworldOf) {
    StarSystem ss;
    ss.scan("Foobar (0, b2g1) -y3");
    EXPECT_EQ(ss.homeworldOf, 0);
    ss.scan("(g1) r1-");
    EXPECT_EQ(ss.homeworldOf, -1);
    ss.scan("Alpha (g1) -y3");
    EXPECT_EQ(ss.homeworldOf, -1);
    ss.scan("Alpha (1,g1) -y3");
    EXPECT_EQ(ss.homeworldOf, 1);
}

TEST(StarSystem, toString) {
    StarSystem ss;
    ss.scan("Foobar (0,b2g1) r1r2-y3b1");
    EXPECT_EQ(ss.toString(), "Foobar (0, g1b2) r1r2-y3b1");
    ss.scan("(g1) r1-");
    EXPECT_EQ(ss.toString(), "(g1) r1-");
    ss.scan("Alpha (g1) -y3");
    EXPECT_EQ(ss.toString(), "Alpha (g1) -y3");
}

TEST(StarSystem, containsOverpopulation) {
    StarSystem ss; ss.scan("Foobar (g1) r1r2g3-y3g1g2b1");
    EXPECT_TRUE(ss.containsOverpopulation());
    EXPECT_FALSE(ss.containsOverpopulation(RED));
    EXPECT_FALSE(ss.containsOverpopulation(YELLOW));
    EXPECT_TRUE(ss.containsOverpopulation(GREEN));
    EXPECT_FALSE(ss.containsOverpopulation(BLUE));
}

TEST(StarSystem, isAdjacentTo) {
    StarSystem s1; s1.scan("Foobar (g1) r2-");
    StarSystem s2; s2.scan("Foobar (g2) r2-");
    StarSystem s3; s3.scan("Foobar (g3) r2-");
    StarSystem s12; s12.scan("Foobar (y1y2) r2-");
    EXPECT_TRUE(s1.isAdjacentTo(s2));
    EXPECT_TRUE(s1.isAdjacentTo(s3));
    EXPECT_TRUE(s2.isAdjacentTo(s1));
    EXPECT_TRUE(s2.isAdjacentTo(s3));
    EXPECT_TRUE(s3.isAdjacentTo(s1));
    EXPECT_TRUE(s3.isAdjacentTo(s2));
    EXPECT_FALSE(s1.isAdjacentTo(s12));
    EXPECT_FALSE(s2.isAdjacentTo(s12));
    EXPECT_TRUE(s3.isAdjacentTo(s12));
    EXPECT_FALSE(s12.isAdjacentTo(s1));
    EXPECT_FALSE(s12.isAdjacentTo(s2));
    EXPECT_TRUE(s12.isAdjacentTo(s3));
}
