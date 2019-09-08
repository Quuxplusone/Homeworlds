
#include "WholeMove.h"
#include <gtest/gtest.h>

#define EXPECT_COMPLETE_MOVE(x) EXPECT_TRUE(x); EXPECT_FALSE(m.isMissingPieces())
#define EXPECT_INCOMPLETE_MOVE(x) EXPECT_TRUE(x); EXPECT_TRUE(m.isMissingPieces())

TEST(WholeMove, ctor) {
    WholeMove m;
    EXPECT_TRUE(m.isPass());
    EXPECT_EQ(m.toString(), "pass");
    EXPECT_EQ(m.toSDGString(), "pass");
}

TEST(WholeMove, scanPass) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("pass"));
    EXPECT_EQ(m.toString(), "pass");
    EXPECT_EQ(m.toSDGString(), "pass");
}

TEST(WholeMove, scanCapture) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("capture r1 at Alpha"));
    EXPECT_EQ(m.toString(), "capture r1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "attack r1 Alpha");

    EXPECT_COMPLETE_MOVE(m.scan("attack r1 Alpha"));
    EXPECT_EQ(m.toString(), "capture r1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "attack r1 Alpha");
}

TEST(WholeMove, scanIncompleteCapture) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("capture")); EXPECT_EQ(m.toString(), "capture");
    EXPECT_INCOMPLETE_MOVE(m.scan("capture r1")); EXPECT_EQ(m.toString(), "capture r1");
    EXPECT_INCOMPLETE_MOVE(m.scan("capture at Alpha")); EXPECT_EQ(m.toString(), "capture at Alpha");
}

TEST(WholeMove, scanMultiCapture) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("sac r2; capture r1r2"));
    EXPECT_EQ(m.toString(), "sacrifice r2; capture r1; capture r2");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac r2; capture r1r2 at Alpha"));
    EXPECT_EQ(m.toString(), "sacrifice r2; capture r1 at Alpha; capture r2 at Alpha");
}

TEST(WholeMove, scanMove) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("move g1 from Alpha to Beta"));
    EXPECT_EQ(m.toString(), "move g1 from Alpha to Beta");
    EXPECT_EQ(m.toSDGString(), "move g1 Alpha Beta");
    EXPECT_FALSE(m.scan("move g1 Alpha Beta"));  // TODO
}

TEST(WholeMove, scanIncompleteMove) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("move g1 to Beta")); EXPECT_EQ(m.toString(), "move g1 to Beta");
    EXPECT_FALSE(m.scan("move g1 from Alpha"));
    EXPECT_FALSE(m.scan("move from Alpha to Beta"));
    EXPECT_FALSE(m.scan("move from Alpha"));
    EXPECT_FALSE(m.scan("move to Beta"));
    EXPECT_FALSE(m.scan("move g1"));
    EXPECT_FALSE(m.scan("move"));
}

TEST(WholeMove, scanMultiMove) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("sac y2 at X; move r1r2 from Alpha to Beta"));
    EXPECT_EQ(m.toString(), "sacrifice y2 at X; move r1 from Alpha to Beta; move r2 from Alpha to Beta");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac y2 at X; move r1r2 to Beta"));
    EXPECT_EQ(m.toString(), "sacrifice y2 at X; move r1 to Beta; move r2 to Beta");
}

TEST(WholeMove, scanDiscover) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("move g1 from Alpha to Beta (b1)"));
    EXPECT_EQ(m.toString(), "move g1 from Alpha to Beta (b1)");
    EXPECT_EQ(m.toSDGString(), "discover g1 Alpha b1 Beta");
    EXPECT_COMPLETE_MOVE(m.scan("discover g1 Alpha b1 Beta"));
    EXPECT_EQ(m.toString(), "move g1 from Alpha to Beta (b1)");
    EXPECT_EQ(m.toSDGString(), "discover g1 Alpha b1 Beta");
    EXPECT_FALSE(m.scan("move g1 from Alpha to Beta (r1b1)"));  // too many pieces in star
}

TEST(WholeMove, scanIncompleteDiscover) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("move g1 to Beta (b1)")); EXPECT_EQ(m.toString(), "move g1 to Beta (b1)");
    EXPECT_FALSE(m.scan("move g1 from Alpha (b1)"));
    EXPECT_FALSE(m.scan("move from Alpha to Beta (b1)"));
    EXPECT_FALSE(m.scan("move from Alpha (b1)"));
    EXPECT_FALSE(m.scan("move to Beta (b1)"));
    EXPECT_FALSE(m.scan("move g1 (b1)"));
    EXPECT_FALSE(m.scan("discover g1 Alpha b1"));
    EXPECT_FALSE(m.scan("discover g1 Alpha Beta"));
    EXPECT_FALSE(m.scan("discover Alpha b1 Beta"));
    EXPECT_FALSE(m.scan("discover g1 b1 Beta"));
    EXPECT_FALSE(m.scan("discover"));
}

TEST(WholeMove, scanMultiDiscover) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("sac y2 at X; move r1r2 from Y to Z (b1)"));
    EXPECT_EQ(m.toString(), "sacrifice y2 at X; move r1 from Y to Z (b1); move r2 from Y to Z");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac y2 at X; move r1r2 to Beta (b1)"));
    EXPECT_EQ(m.toString(), "sacrifice y2 at X; move r1 to Beta (b1); move r2 to Beta");
}

TEST(WholeMove, scanBuild) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("build g1 at Alpha"));
    EXPECT_EQ(m.toString(), "build g1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "build g1 Alpha");
    EXPECT_COMPLETE_MOVE(m.scan("build g1 Alpha"));
    EXPECT_EQ(m.toString(), "build g1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "build g1 Alpha");
}

TEST(WholeMove, scanIncompleteBuild) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("build at Alpha")); EXPECT_EQ(m.toString(), "build at Alpha");
    EXPECT_INCOMPLETE_MOVE(m.scan("build Alpha")); EXPECT_EQ(m.toString(), "build at Alpha");
    EXPECT_INCOMPLETE_MOVE(m.scan("build g1")); EXPECT_EQ(m.toString(), "build g1");
    EXPECT_INCOMPLETE_MOVE(m.scan("build 3")); EXPECT_EQ(m.toString(), "build 3");
}

TEST(WholeMove, scanMultiBuild) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("sac g2 at X; build r1r2 at Alpha"));
    EXPECT_EQ(m.toString(), "sacrifice g2 at X; build r1 at Alpha; build r2 at Alpha");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac g2 at X; build r1r2"));
    EXPECT_EQ(m.toString(), "sacrifice g2 at X; build r1; build r2");
}

TEST(WholeMove, scanTrade) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("convert g1 to b1 at Alpha"));
    EXPECT_EQ(m.toString(), "convert g1 to b1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "trade g1 b1 Alpha");
    EXPECT_COMPLETE_MOVE(m.scan("trade g1 b1 Alpha"));
    EXPECT_EQ(m.toString(), "convert g1 to b1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "trade g1 b1 Alpha");
    EXPECT_COMPLETE_MOVE(m.scan("trade g1 for b1 at Alpha"));
    EXPECT_EQ(m.toString(), "convert g1 to b1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "trade g1 b1 Alpha");
}

TEST(WholeMove, scanNoopTrade) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("convert b1 to b1 at Alpha"));
    EXPECT_EQ(m.toString(), "convert b1 to b1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "trade b1 b1 Alpha");
    EXPECT_COMPLETE_MOVE(m.scan("trade b1 b1 Alpha"));
    EXPECT_EQ(m.toString(), "convert b1 to b1 at Alpha");
    EXPECT_EQ(m.toSDGString(), "trade b1 b1 Alpha");
}

TEST(WholeMove, scanImpossibleTrade) {
    WholeMove m;
    EXPECT_FALSE(m.scan("convert b1 to g2 at Alpha"));
    EXPECT_FALSE(m.scan("convert 1 to 2 at Alpha"));
    EXPECT_FALSE(m.scan("convert b2 to g1 at Alpha"));
    EXPECT_FALSE(m.scan("convert 2 to 1 at Alpha"));
}

TEST(WholeMove, scanIncompleteTrade) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("convert g1 to b1")); EXPECT_EQ(m.toString(), "convert g1 to b1");
    EXPECT_INCOMPLETE_MOVE(m.scan("trade g1 for b1")); EXPECT_EQ(m.toString(), "convert g1 to b1");
    EXPECT_INCOMPLETE_MOVE(m.scan("convert g1 b1")); EXPECT_EQ(m.toString(), "convert g1 to b1");
    EXPECT_INCOMPLETE_MOVE(m.scan("trade g1 b1")); EXPECT_EQ(m.toString(), "convert g1 to b1");
    EXPECT_INCOMPLETE_MOVE(m.scan("convert to b1 at Alpha")); EXPECT_EQ(m.toString(), "convert 1 to b1 at Alpha");
    EXPECT_INCOMPLETE_MOVE(m.scan("trade for b1 at Alpha")); EXPECT_EQ(m.toString(), "convert 1 to b1 at Alpha");
    EXPECT_INCOMPLETE_MOVE(m.scan("convert to b1")); EXPECT_EQ(m.toString(), "convert 1 to b1");
    EXPECT_INCOMPLETE_MOVE(m.scan("trade for b1")); EXPECT_EQ(m.toString(), "convert 1 to b1");
    EXPECT_INCOMPLETE_MOVE(m.scan("convert 1 to b1")); EXPECT_EQ(m.toString(), "convert 1 to b1");
    EXPECT_FALSE(m.scan("convert b1 at Alpha"));
    EXPECT_FALSE(m.scan("trade b1 at Alpha"));
    EXPECT_FALSE(m.scan("trade b1 Alpha"));
}

TEST(WholeMove, scanMultiTrade) {
    WholeMove m;
    EXPECT_FALSE(m.scan("sac b2; convert r1r2 to b1b2 at Alpha"));
    EXPECT_FALSE(m.scan("sac b2; convert r1r2 to b1b3 at Alpha"));
}

TEST(WholeMove, scanSacrifice) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("sacrifice g3 at Foo"));
    EXPECT_EQ(m.toString(), "sacrifice g3 at Foo");
    EXPECT_EQ(m.toSDGString(), "sacrifice g3 Foo; pass; pass; pass");
    EXPECT_COMPLETE_MOVE(m.scan("sacrifice g3 Foo"));
    EXPECT_EQ(m.toString(), "sacrifice g3 at Foo");
    EXPECT_EQ(m.toSDGString(), "sacrifice g3 Foo; pass; pass; pass");
    EXPECT_COMPLETE_MOVE(m.scan("sac g3 at Foo"));
    EXPECT_EQ(m.toString(), "sacrifice g3 at Foo");
    EXPECT_EQ(m.toSDGString(), "sacrifice g3 Foo; pass; pass; pass");
    EXPECT_COMPLETE_MOVE(m.scan("sac g3 Foo"));
    EXPECT_EQ(m.toString(), "sacrifice g3 at Foo");
    EXPECT_EQ(m.toSDGString(), "sacrifice g3 Foo; pass; pass; pass");
    EXPECT_FALSE(m.scan("sac g1g2 at Foo"));
}

TEST(WholeMove, scanIncompleteSacrifice) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("sac g3"));
    EXPECT_EQ(m.toString(), "sacrifice g3");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac; move r1 from Alpha to Beta"));
    EXPECT_EQ(m.toString(), "sacrifice; move r1 from Alpha to Beta");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac 2; move r1 from Alpha to Beta"));
    EXPECT_EQ(m.toString(), "sacrifice 2; move r1 from Alpha to Beta");
    EXPECT_INCOMPLETE_MOVE(m.scan("sac y; move r1 from Alpha to Beta"));
    EXPECT_EQ(m.toString(), "sacrifice y; move r1 from Alpha to Beta");
}

TEST(WholeMove, scanSacrificeByColorName) {
    // TODO: this is surprising behavior
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("sac red")); EXPECT_EQ(m.toString(), "sacrifice at red");
    EXPECT_FALSE(m.scan("sac red at Alpha"));
}

TEST(WholeMove, scanImpossibleSacrifice) {
    WholeMove m;
    EXPECT_FALSE(m.scan("sac r1; move r1 from Alpha to Beta"));  // wrong color
    EXPECT_FALSE(m.scan("sac r1; capture r1 at Beta; capture r2 at Beta"));  // too many moves
    EXPECT_FALSE(m.scan("sac r1; capture r1 at Beta; pass"));  // too many moves
}

TEST(WholeMove, scanSacrificeWithExplicitPass) {
    WholeMove m;
    EXPECT_FALSE(m.scan("sac r1; pass"));  // explicit passes are not allowed
    EXPECT_FALSE(m.scan("sac r2; pass; pass"));  // explicit passes are not allowed
}

TEST(WholeMove, scanCatastrophe) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("catastrophe red at Alpha"));
    EXPECT_EQ(m.toString(), "catastrophe red at Alpha");
    EXPECT_EQ(m.toSDGString(), "catastrophe Alpha red");
    EXPECT_COMPLETE_MOVE(m.scan("catastrophe y at Alpha"));
    EXPECT_EQ(m.toString(), "catastrophe yellow at Alpha");
    EXPECT_EQ(m.toSDGString(), "catastrophe Alpha yellow");
    EXPECT_COMPLETE_MOVE(m.scan("catastrophe Alpha red"));
    EXPECT_EQ(m.toString(), "catastrophe red at Alpha");
    EXPECT_EQ(m.toSDGString(), "catastrophe Alpha red");
    EXPECT_COMPLETE_MOVE(m.scan("cat Alpha red"));
    EXPECT_EQ(m.toString(), "catastrophe red at Alpha");
    EXPECT_EQ(m.toSDGString(), "catastrophe Alpha red");
    EXPECT_COMPLETE_MOVE(m.scan("cat Alpha r"));
    EXPECT_EQ(m.toString(), "catastrophe red at Alpha");
    EXPECT_EQ(m.toSDGString(), "catastrophe Alpha red");
    EXPECT_FALSE(m.scan("catastrophe 2 at Alpha"));
    EXPECT_FALSE(m.scan("catastrophe g2 at Alpha"));
    EXPECT_FALSE(m.scan("catastrophe Alpha"));
    EXPECT_FALSE(m.scan("catastrophe Alpha at red"));
}

TEST(WholeMove, scanPreCatastrophe) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("catastrophe red at Alpha; build g1 at Beta"));
    EXPECT_EQ(m.toString(), "catastrophe red at Alpha; build g1 at Beta");
    EXPECT_EQ(m.toSDGString(), "catastrophe Alpha red; build g1 Beta");
}

TEST(WholeMove, scanPostCatastrophe) {
    WholeMove m;
    EXPECT_COMPLETE_MOVE(m.scan("build g1 at Beta; catastrophe red at Alpha"));
    EXPECT_EQ(m.toString(), "build g1 at Beta; catastrophe red at Alpha");
    EXPECT_EQ(m.toSDGString(), "build g1 Beta; catastrophe Alpha red");
}

TEST(WholeMove, scanIncompleteCatastrophe) {
    WholeMove m;
    EXPECT_INCOMPLETE_MOVE(m.scan("catastrophe red")); EXPECT_EQ(m.toString(), "catastrophe red");
    EXPECT_INCOMPLETE_MOVE(m.scan("cat red")); EXPECT_EQ(m.toString(), "catastrophe red");
    EXPECT_INCOMPLETE_MOVE(m.scan("cat r")); EXPECT_EQ(m.toString(), "catastrophe red");
    EXPECT_INCOMPLETE_MOVE(m.scan("catastrophe")); EXPECT_EQ(m.toString(), "catastrophe");
    EXPECT_INCOMPLETE_MOVE(m.scan("cat")); EXPECT_EQ(m.toString(), "catastrophe");
    EXPECT_INCOMPLETE_MOVE(m.scan("catastrophe at Alpha")); EXPECT_EQ(m.toString(), "catastrophe at Alpha");
}
