
#include "PieceCollection.h"
#include <gtest/gtest.h>

TEST(Piece, toString) {
    const char *s = Piece(RED, SMALL).toString();
    EXPECT_STREQ(s, "r1");
    EXPECT_STREQ(Piece(RED, MEDIUM).toString(), "r2");
    EXPECT_STREQ(Piece(RED, LARGE).toString(), "r3");
    EXPECT_STREQ(Piece(RED, UNKNOWN_SIZE).toString(), "r");
    EXPECT_STREQ(Piece(YELLOW, SMALL).toString(), "y1");
    EXPECT_STREQ(Piece(YELLOW, MEDIUM).toString(), "y2");
    EXPECT_STREQ(Piece(YELLOW, LARGE).toString(), "y3");
    EXPECT_STREQ(Piece(YELLOW, UNKNOWN_SIZE).toString(), "y");
    EXPECT_STREQ(Piece(GREEN, SMALL).toString(), "g1");
    EXPECT_STREQ(Piece(GREEN, MEDIUM).toString(), "g2");
    EXPECT_STREQ(Piece(GREEN, LARGE).toString(), "g3");
    EXPECT_STREQ(Piece(GREEN, UNKNOWN_SIZE).toString(), "g");
    EXPECT_STREQ(Piece(BLUE, SMALL).toString(), "b1");
    EXPECT_STREQ(Piece(BLUE, MEDIUM).toString(), "b2");
    EXPECT_STREQ(Piece(BLUE, LARGE).toString(), "b3");
    EXPECT_STREQ(Piece(BLUE, UNKNOWN_SIZE).toString(), "b");
    EXPECT_STREQ(Piece(UNKNOWN_COLOR, SMALL).toString(), "1");
    EXPECT_STREQ(Piece(UNKNOWN_COLOR, MEDIUM).toString(), "2");
    EXPECT_STREQ(Piece(UNKNOWN_COLOR, LARGE).toString(), "3");
    EXPECT_STREQ(Piece(UNKNOWN_COLOR, UNKNOWN_SIZE).toString(), "");
}

TEST(Piece, isMissingPieces) {
    EXPECT_FALSE(Piece(RED, SMALL).isMissingPieces());
    EXPECT_FALSE(Piece(YELLOW, MEDIUM).isMissingPieces());
    EXPECT_TRUE(Piece(YELLOW, UNKNOWN_SIZE).isMissingPieces());
    EXPECT_TRUE(Piece(UNKNOWN_COLOR, MEDIUM).isMissingPieces());
    EXPECT_TRUE(Piece(UNKNOWN_COLOR, UNKNOWN_SIZE).isMissingPieces());
}

TEST(Piece, isEmpty) {
    EXPECT_FALSE(Piece(RED, SMALL).empty());
    EXPECT_FALSE(Piece(YELLOW, MEDIUM).empty());
    EXPECT_FALSE(Piece(YELLOW, UNKNOWN_SIZE).empty());
    EXPECT_FALSE(Piece(UNKNOWN_COLOR, MEDIUM).empty());
    EXPECT_TRUE(Piece(UNKNOWN_COLOR, UNKNOWN_SIZE).empty());
}

TEST(PieceCollection, defaultCtor) {
    PieceCollection pc;
    EXPECT_TRUE(pc.empty());
    EXPECT_EQ(pc.number(), 0);
    EXPECT_EQ(pc.toString(), "");
}

TEST(PieceCollection, clear) {
    PieceCollection pc;
    pc.insert(RED, SMALL);
    EXPECT_FALSE(pc.empty());
    pc.clear();
    EXPECT_TRUE(pc.empty());
}

TEST(PieceCollection, accessors) {
    PieceCollection r1r2y1y3y3b2; EXPECT_TRUE(r1r2y1y3y3b2.scan("r1r2y1y3y3b2"));
    PieceCollection r1y3b2; r1y3b2.scan("r1y3b2");
    EXPECT_TRUE(r1y3b2.contains(r1y3b2));
    EXPECT_TRUE(r1r2y1y3y3b2.contains(r1y3b2));
    EXPECT_FALSE(r1y3b2.contains(r1r2y1y3y3b2));
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(RED), 2);
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(YELLOW), 3);
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(GREEN), 0);
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(BLUE), 1);
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(SMALL), 2);
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(MEDIUM), 2);
    EXPECT_EQ(r1r2y1y3y3b2.numberOf(LARGE), 2);
    EXPECT_EQ(r1r2y1y3y3b2.number(), 6);
}

TEST(PieceCollection, numberAtLeast) {
    PieceCollection r1r2y1y3y3b2; EXPECT_TRUE(r1r2y1y3y3b2.scan("r1r2y1y3y3b2"));
    EXPECT_EQ(r1r2y1y3y3b2.numberAtLeast(SMALL), 6);
    EXPECT_EQ(r1r2y1y3y3b2.numberAtLeast(MEDIUM), 4);
    EXPECT_EQ(r1r2y1y3y3b2.numberAtLeast(LARGE), 2);
    PieceCollection y2g3b3; y2g3b3.scan("y2g3b3");
    EXPECT_EQ(y2g3b3.numberAtLeast(SMALL), 3);
    EXPECT_EQ(y2g3b3.numberAtLeast(MEDIUM), 3);
    EXPECT_EQ(y2g3b3.numberAtLeast(LARGE), 2);
    PieceCollection r1y2g1; r1y2g1.scan("r1y2g1");
    EXPECT_EQ(r1y2g1.numberAtLeast(SMALL), 3);
    EXPECT_EQ(r1y2g1.numberAtLeast(MEDIUM), 1);
    EXPECT_EQ(r1y2g1.numberAtLeast(LARGE), 0);
}

TEST(PieceCollection, smallestSizeOf) {
    PieceCollection r1r2y3y3b2; EXPECT_TRUE(r1r2y3y3b2.scan("r1r2y3y3b2"));
    EXPECT_EQ(r1r2y3y3b2.smallestSizeOf(RED), SMALL);
    EXPECT_EQ(r1r2y3y3b2.smallestSizeOf(YELLOW), LARGE);
    EXPECT_EQ(r1r2y3y3b2.smallestSizeOf(BLUE), MEDIUM);
}

TEST(PieceCollection, toString) {
    PieceCollection r1r2y3y3b2;
    EXPECT_TRUE(r1r2y3y3b2.scan("r1r2y3y3b2"));
    EXPECT_EQ(r1r2y3y3b2.toString(), "r1r2y3y3b2");
}

TEST(PieceCollection, scanEmpty) {
    PieceCollection empty;
    EXPECT_TRUE(empty.scan(""));
    EXPECT_TRUE(empty.empty());
}
