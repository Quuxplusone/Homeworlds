
#include "AllMoves.h"
#include "ApplyMove.h"
#include "InferMove.h"
#include "WholeMove.h"
#include <gtest/gtest.h>

static bool can_reach(const GameState& oldst, bool only_wins, const GameState& targetst)
{
    std::string target = targetst.toComparableString();
    return findAllMoves(
        oldst, 0,
        false, only_wins, 0xF,
        [&](const WholeMove&, const GameState& newst) {
            return (newst.toComparableString() == target);
        }
    );
}

#define EXPECT_WINNING_MOVE(text) do { \
        WholeMove m; \
        GameState newst = st; \
        EXPECT_TRUE(m.scan(text)); \
        EXPECT_TRUE(m.sanitycheck()); \
        EXPECT_TRUE(inferMoveFromState(newst, 0, &m)); \
        EXPECT_EQ(ApplyMove::Whole(newst, 0, m), ApplyMove::Result::SUCCESS); \
        EXPECT_TRUE(newst.gameIsOver()); \
        EXPECT_TRUE(newst.hasLost(1)); \
        EXPECT_TRUE(can_reach(st, true, newst)); \
    } while (0)

#define EXPECT_WINNING_BUT_OVERCOMPLICATED_MOVE(text) do { \
        WholeMove m; \
        GameState newst = st; \
        EXPECT_TRUE(m.scan(text)); \
        EXPECT_TRUE(m.sanitycheck()); \
        EXPECT_TRUE(inferMoveFromState(newst, 0, &m)); \
        EXPECT_EQ(ApplyMove::Whole(newst, 0, m), ApplyMove::Result::SUCCESS); \
        EXPECT_TRUE(newst.gameIsOver()); \
        EXPECT_TRUE(newst.hasLost(1)); \
        EXPECT_TRUE(can_reach(st, false, newst)); \
    } while (0)

#define EXPECT_LEGAL_MOVE(text) do { \
        WholeMove m; \
        GameState newst = st; \
        EXPECT_TRUE(m.scan(text)); \
        EXPECT_TRUE(m.sanitycheck()); \
        EXPECT_TRUE(inferMoveFromState(newst, 0, &m)); \
        EXPECT_EQ(ApplyMove::Whole(newst, 0, m), ApplyMove::Result::SUCCESS); \
        EXPECT_FALSE(newst.gameIsOver()); \
        EXPECT_TRUE(can_reach(st, false, newst)); \
    } while (0)

#define EXPECT_ILLEGAL_MOVE(text) do { \
        WholeMove m; \
        EXPECT_TRUE(m.scan(text)); \
        EXPECT_TRUE(m.sanitycheck()); \
        EXPECT_TRUE(inferMoveFromState(st, 0, &m)); \
        EXPECT_FALSE(ApplyMove::isValidMove(st, 0, m)); \
    } while (0)

#define EXPECT_AMBIGUOUS_MOVE(text) do { \
        WholeMove m; \
        EXPECT_TRUE(m.scan(text)); \
        EXPECT_TRUE(m.sanitycheck()); \
        EXPECT_FALSE(inferMoveFromState(st, 0, &m)); \
    } while (0)

static GameState from(const char *text)
{
    GameState st;
    EXPECT_TRUE(st.scan(text));
    return st;
}

TEST(AllMoves, nowin_and_cant_avoid_catastrophe) {
    GameState st = from(R"(
        Player1 (0, r3g1) r1r1r2r3-
        Player2 (1, y2g1) -b3
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_but_avoid_losing_by_catastrophe) {
    GameState st = from(R"(
        Player1 (0, r3g1) r1r2r3-
        Player2 (1, y2g1) -b3
    )");
    EXPECT_LEGAL_MOVE("sacrifice r1");
    EXPECT_LEGAL_MOVE("sacrifice r2");
    EXPECT_LEGAL_MOVE("sacrifice r3");
    EXPECT_LEGAL_MOVE("pass");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_build_precatastrophe_other) {
    GameState st = from(R"(
        Player1 (0, y3g2) y1-
        CantCatastropheHere (r2) r1r1g1-r3
        Redworld (r3) -r2
        Player2 (1, r3b2) r1-r2b1b1b1
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_build_sacrifice_medium) {
    GameState st = from(R"(
        Player1 (0, y1g3) g1g1-
        Beta (g3) -r1
        Gamma (g3) -y1g2
        Delta (y1) g2-
        Player2 (1, y3b1) g2-g1
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_capture_catastrophe) {
    GameState st = from(R"(
        Player1 (0, y3g2) y1-
        Player2 (1, r3b2) r1-r2b1b1b1
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_capture_sacrifice_medium_red) {
    GameState st = from(R"(
        Player1 (0, g3y2) r2-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) r2-y2y2
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_move_sacrifice_medium_yellow_waypoint) {
    GameState st = from(R"(
        Player1 (0, b3g1) y2g2r1r1r1g1b1-
        CantCreateSmallWaypoint (b2) y1g1b1b1-
        Player2 (1, r3b2) -y1y1y3
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_simultaneous_catastrophe_2) {
    // This is similar to p0-win-by-avoid-postcatastrophe-4.
    // Player1 needs to eliminate his red overpopulation while simultaneously
    // preserving the ability to catastrophe blue at Player2. But he can't.
    GameState st = from(R"(
        Player1 (0, r3) r2r2r3-r1
        Alpha (y2) g2-y2g2b2
        Beta (b2) -r2y2g2
        Player2 (1, b3) b3-g3g3b2b1b1
    )");
    EXPECT_ILLEGAL_MOVE("catastrophe red at Player1");
    EXPECT_ILLEGAL_MOVE("catastrophe blue at Player2");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture g3g3 at Player2");
    EXPECT_ILLEGAL_MOVE("sacrifice r3; capture g3g3 at Player2; catastrophe blue at Player2");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_simultaneous_catastrophe) {
    // Player1 cannot avert a blue catastrophe at his homeworld.
    // Even if he causes a catastrophe at Player2, Player2 still
    // has the option to cause the catastrophe at Player1 ---
    // and in fact vice versa. Neither player has a winning move.
    GameState st = from(R"(
        Player1 (0, b3g1) b1b1b1b3-
        Alpha (y3) -g3g3g3y3y3r3r3r3
        Beta (y1) -r1r1r1y1y1g1g1
        Player2 (1, b3) b2b2b2-g2y2y2y2
    )");
    EXPECT_ILLEGAL_MOVE("catastrophe blue at Player1");
    EXPECT_ILLEGAL_MOVE("catastrophe blue at Player2");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_swap_ships) {
    GameState st = from(R"(
        Player1 (0, b2g1) r1y1g1b2-
        Alpha (b1) -r1y1y1b1
        Player2 (1, y3g1) r1b1-y3y2
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_by_temporarily_abandon_homeworld) {
    GameState st = from(R"(
        Player1 (0, g3y1) y2-
        Alpha (b2) g1-
        Beta (b1) g1-
        Player2 (1, g3y2) -g3g1
    )");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_doomed_homeworld) {
    GameState st = from(R"(
        Player1 (0, g3r1) r1r2r2g1g1g2-r1g1
        Player2 (1, g3) y1y1y1-y3
    )");
    EXPECT_ILLEGAL_MOVE("catastrophe yellow at Player2; catastrophe red at Player1; catastrophe green at Player1");
    EXPECT_ILLEGAL_MOVE("catastrophe yellow at Player2; catastrophe red at Player1");
    EXPECT_ILLEGAL_MOVE("catastrophe yellow at Player2; catastrophe green at Player1");
    EXPECT_ILLEGAL_MOVE("catastrophe red at Player1; catastrophe green at Player1");
    EXPECT_ILLEGAL_MOVE("catastrophe yellow at Player2");
    EXPECT_LEGAL_MOVE("catastrophe red at Player1");
    EXPECT_LEGAL_MOVE("catastrophe green at Player1");
    EXPECT_LEGAL_MOVE("move y1 to Alpha (b2)");
    EXPECT_LEGAL_MOVE("move y1 to Alpha (b2); catastrophe green at Player1");
    EXPECT_LEGAL_MOVE("sacrifice g1");
    EXPECT_LEGAL_MOVE("sacrifice g1; build y2; catastrophe red at Player1");
    EXPECT_LEGAL_MOVE("pass");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_trivial_1) {
    GameState st = from(R"(
        Player1 (0, y1b2) r3g1-
        Player2 (1, y1b3) -r1y2y3g1
        Alpha (g3) y1-
    )");
    EXPECT_LEGAL_MOVE("build y2 at Alpha");
    EXPECT_LEGAL_MOVE("build r1 at Player1");
    EXPECT_LEGAL_MOVE("build g1 at Player1");
    EXPECT_ILLEGAL_MOVE("build g2 at Player1");
    EXPECT_ILLEGAL_MOVE("build y2 at Player1");
    EXPECT_AMBIGUOUS_MOVE("sacrifice");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, nowin_trivial_multicapture) {
    GameState st = from(R"(
        Player1 (0, y1b3) r3g1-
        Player2 (1, y1b3) -y2y3
        Alpha (g3) g2-r2r2y2b2
        Beta (g1) g2-y2b2
        Gamma (b1) g2-b2
    )");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture r2r2");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture y2y2");
    EXPECT_AMBIGUOUS_MOVE("sacrifice r3; capture b2b2");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture r2y2y2");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture y2y2; capture r2 at Alpha");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture b2 at Gamma; capture y2y2");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture y2y2; capture b2 at Gamma");
    EXPECT_LEGAL_MOVE("sacrifice r3; capture b2 at Beta; capture y2y2");
    EXPECT_FALSE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_avoid_postcatastrophe_1) {
    // The winning tactic here is some combination of sacrificing and
    // converting Player1's medium blues to eliminate that overpopulation,
    // followed by causing a catastrophe at Player2.
    GameState st = from(R"(
        Player1 (0, b3) b2b2b2-
        Player2 (1, g3) -g2g2g2
    )");
    EXPECT_WINNING_MOVE("sacrifice b2; catastrophe");
    EXPECT_WINNING_MOVE("convert to r2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_avoid_postcatastrophe_2) {
    GameState st = from(R"(
        Player1 (0, b3) b2b2b2-
        Player2 (1, g3) b1-g2g2
    )");
    EXPECT_WINNING_MOVE("sacrifice b2; convert to g1; catastrophe green");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_avoid_postcatastrophe_3) {
    // Player1 needs to eliminate his red overpopulation while simultaneously
    // preserving the ability to catastrophe blue at Player2.
    GameState st = from(R"(
        Player1 (0, r3) r2r2r2r3-
        Player2 (1, b3) b1b2-b2b2
    )");
    EXPECT_WINNING_MOVE("sacrifice b2; convert r2 to y2; convert r2 to y2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_avoid_postcatastrophe_4) {
    // Player1 needs to eliminate his red overpopulation while simultaneously
    // preserving the ability to catastrophe blue at Player2. To complicate
    // matters, he actually needs to swap a ship somewhere else in the galaxy
    // in order to free up a non-red ship.
    GameState st = from(R"(
        Player1 (0, r3) r2r2r3-r1
        Alpha (y2) g2-y2g2b2
        Beta (b2) -y2g2
        Player2 (1, b3) b3-g3g3b2b1b1
    )");
    EXPECT_WINNING_MOVE("sacrifice b3; convert r3 to y3; convert to r2 at Alpha; convert to g2 at Player1; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build) {
    GameState st = from(R"(
        Player1 (0, y1g3) g2-
        Player2 (1, y3g2) b1-b2b1
    )");
    EXPECT_WINNING_MOVE("build b1; catastrophe blue at Player2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_medium) {
    GameState st = from(R"(
        Player1 (0, y1g3) g2b1-
        Player2 (1, y3g2) b1-b2b1
    )");
    EXPECT_WINNING_MOVE("build b2 at Player2; catastrophe");
    EXPECT_ILLEGAL_MOVE("build b1 at Player2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_precatastrophe_homeworld) {
    GameState st = from(R"(
        PreCatastropheMyHomeworld (0, y3r2) y1r1r1-r3
        SacrificeHere (r3) g1-r2
        BuildRedHere (1, r3b2) r1-r2b1b1b1
    )");
    EXPECT_AMBIGUOUS_MOVE("catastrophe red; sacrifice g1; build r1; catastrophe");
    EXPECT_LEGAL_MOVE("catastrophe red; sacrifice g1; build r1; catastrophe red");
    EXPECT_WINNING_MOVE("catastrophe red; sacrifice g1; build r1; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_precatastrophe_other) {
    GameState st = from(R"(
        Player1 (0, y3g2) y1-
        PreCatastropheHere (r2) r1r1-r3
        SacrificeHere (r3) g1-r2
        BuildRedHere (1, r3b2) r1-r2b1b1b1
    )");
    EXPECT_WINNING_MOVE("catastrophe red; sacrifice g1; build r1; catastrophe red; catastrophe blue");
    EXPECT_LEGAL_MOVE("catastrophe red; sacrifice g1; build r1; catastrophe red");
    EXPECT_LEGAL_MOVE("catastrophe red; sacrifice g1; build r1; catastrophe blue");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_precatastrophe_sacloc) {
    GameState st = from(R"(
        Player1 (0, y3g2) g1b1-b3
        Alpha (r2) g2b1-b1b2b2b3
        Player2 (1, b2) b3-r1y1g1
    )");
    EXPECT_WINNING_MOVE("catastrophe blue; sacrifice g2; build b1b1 at Player2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_precatastrophe_twice) {
    GameState st = from(R"(
        Player1 (0, y3g2) g1b1-b3
        PreCatastropheHere (r2) r1b1-r1r3
        PreCatastropheHereToo (r2) b1-r2r3r3
        Player2 (1, b2) b3g2-r1y1g1
    )");
    EXPECT_WINNING_MOVE("catastrophe; catastrophe; sacrifice; build b1b1 at Player2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_red_postcatastrophe) {
    GameState st = from(R"(
        Player1 (0, y3g2) y1g1-
        Player2 (1, r3b2) r1-r2b1b1b1
    )");
    EXPECT_WINNING_MOVE("sacrifice g1; build r1; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_sacrifice_medium_green) {
    GameState st = from(R"(
        Player1 (0, y1g3) g2b1-
        Player2 (1, y3g2) b1-b2
    )");
    EXPECT_WINNING_MOVE("sacrifice g2; build b1b2 at Player2; catastrophe blue");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_sacrifice_medium_and_rebuild_star) {
    GameState st = from(R"(
        Player1 (0, y1g3) g1-
        Alpha (g1) g2-
        Beta (g3) -r1
        Gamma (g3) -y1g2
        Player2 (1, y3b1) g2-g1
    )");
    EXPECT_WINNING_MOVE("sacrifice at Alpha; build g1g2 at Player2; catastrophe green");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_sacrifice_medium_green_to_catastrophe_star) {
    GameState st = from(R"(
        Player1 (0, y1g3) g2b1-
        Player2 (1, y3g2) y1y1g1g1-r3
    )");
    EXPECT_WINNING_MOVE("sacrifice g2; build y2g1 at Player2; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_sacrifice_small_and_rebuild_1) {
    GameState st = from(R"(
        Player1 (0, y1g3) g1-
        Alpha (b2) g1-
        Beta (g3) -r1
        Gamma (g3) -y1g2
        Player2 (1, y3g2) g2-g1
    )");
    EXPECT_WINNING_MOVE("sacrifice g1 at Alpha; build g1 at Player2; catastrophe green at Player2");
    EXPECT_ILLEGAL_MOVE("sacrifice g1 at Player1; build g1 at Player2; catastrophe green at Player2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_sacrifice_small_and_rebuild_2) {
    GameState st = from(R"(
        Player1 (0, y1g3) g1g1-
        Beta (g3) -r1
        Gamma (g3) -y1g2
        Player2 (1, y3g2) g2-g1
    )");
    EXPECT_WINNING_MOVE("sacrifice g1 at Player1; build at Player2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_build_tricky_catastrophes) {
    GameState st = from(R"(
        Player1 (0, y3g1) g2g2-y1y1y2g3
        Alpha (y2) y2-
        Beta (g2) -g3
        Player2 (1, y3g1) y1g1-y3g3
    )");
    EXPECT_WINNING_MOVE("catastrophe yellow; sacrifice g2; build y1g2 at Player2; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_catastrophe) {
    GameState st = from(R"(
        Player1 (0, y3g2) y1-
        Player2 (1, r3b2) r1-r1b1b1b1
    )");
    EXPECT_WINNING_MOVE("capture r1; catastrophe blue");
    EXPECT_LEGAL_MOVE("capture r1");
    EXPECT_LEGAL_MOVE("capture b1");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_equal_size) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) r2-y2
    )");
    EXPECT_WINNING_MOVE("capture y2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_sacrifice_medium_red) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3r2-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) g2-y2y2
    )");
    EXPECT_WINNING_MOVE("sacrifice; capture y2y2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_sacrifice_medium_red_at_target) {
    GameState st = from(R"(
        Player1 (0, g3y2) r2-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) g2r2-y2y2
    )");
    EXPECT_WINNING_MOVE("sacrifice at Player2; capture y2y2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_sacrifice_small_red) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3r1-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) g2-y2
    )");
    EXPECT_WINNING_MOVE("sacrifice r1; capture");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_with_small_red) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) g2r1-y2
    )");
    EXPECT_WINNING_MOVE("capture y2");
    EXPECT_WINNING_MOVE("sacrifice r1; capture y2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture_with_star_power) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, r2g1) g2-y2
    )");
    EXPECT_WINNING_MOVE("capture y2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_capture) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, b2g1) r3-y2
    )");
    EXPECT_WINNING_MOVE("capture");
    EXPECT_WINNING_MOVE("capture y2");
    EXPECT_WINNING_MOVE("capture y2 at Player2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_cat_create_waypoint) {
    GameState st = from(R"(
        Player1 (0, b3g1) y2g2r1r1r1r2g1b1-
        Alpha (b2) y1g1b1b1-
        Player2 (1, r3b2) -y1y1y3
    )");
    EXPECT_WINNING_MOVE("catastrophe red; sacrifice y2; move y1 from Alpha to Waypoint (r1); move y1 to Player2; catastrophe yellow");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_cat_unneeded_star) {
    GameState st = from(R"(
        Player1 (0, b3g1) b1-
        Alpha (b2) b3b2b1-
        Player2 (1, y3b1) b2g1-b3
    )");
    EXPECT_WINNING_MOVE("catastrophe at Alpha; build b1 at Player2; catastrophe blue");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade_away_from_needed_color_1) {
    GameState st = from(R"(
        Player1 (0, g2y1) b2b3-
        Yellow (y2) y2-
        Player2 (1, r3y1) r2r1r1g2g2-y3b1
    )");
    EXPECT_WINNING_MOVE("catastrophe; sacrifice; convert y2 to r2; convert g2 to y2; convert g2 to y2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade_away_from_needed_color_2) {
    GameState st = from(R"(
        Player1 (0, g1r2) y2-
        Alpha (b1) -y1
        Player2 (1, y1r3) b1y1r1r2-y3r1r3
    )");
    EXPECT_WINNING_MOVE("convert r2 to y2 at Player2; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade_away_from_unneeded_fivesome) {
    GameState st = from(R"(
        Player1 (0, b1g3) y3-
        Player2 (1, y3b2) r2r2y2y1y1y1-r3
    )");
    EXPECT_WINNING_MOVE("convert y1 to r1; catastrophe red");
    EXPECT_WINNING_MOVE("convert y1 to r1; catastrophe yellow; catastrophe red");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade_away_from_unneeded_foursome) {
    GameState st = from(R"(
        Player1 (0, b1g3) y3-
        Player2 (1, y3b2) r2r2y1y1y1-r3
    )");
    EXPECT_WINNING_MOVE("convert y1 to r1; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade_sacrifice_blue_to_catastrophe_blue) {
    GameState st = from(R"(
        Player1 (0, b3y1) r1-
        Alpha (b1) r1-
        Beta (b1) g1-
        Gamma (b2) r1-
        Delta (b2) g1-
        Player2 (1, b3) b3y3y2g1-r2g2
    )");
    EXPECT_WINNING_MOVE("sacrifice b3; convert to b3; convert to b2; convert to b1 at Player2; catastrophe blue");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade_sacrifice_medium_blue) {
    GameState st = from(R"(
        Player1 (0, g3y2) r1-
        Earth (g2) -r1
        Fire (g2) -y1
        Air (g1) -r1
        Water (g1) -b1
        Player2 (1, b1g3) b2b2g2g3-r3r2
    )");
    EXPECT_LEGAL_MOVE("sacrifice b2; convert g2 to b2; convert g3 to b3; catastrophe");
    EXPECT_WINNING_MOVE("sacrifice b2; convert g2 to r2; convert g3 to r3; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_trade) {
    GameState st = from(R"(
        Player1 (0, g3y2) g3-
        Blueberry (b3) y1y1g1-
        Timbuktu (g2) -r1g1
        Player2 (1, y2b1) r3-y2y1
    )");
    EXPECT_WINNING_MOVE("convert r3 to y3; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_move_sacrifice_medium_yellow_1) {
    GameState st = from(R"(
        Player1 (0, b3g1) y2g2-
        Alpha (g1) y1b1-
        Player2 (1, y3b2) -b1b1y1y1r3
    )");
    EXPECT_WINNING_MOVE("sacrifice y2; move y1b1 to Player2; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_move_sacrifice_medium_yellow_2) {
    GameState st = from(R"(
        Player1 (0, b3g1) y2g2-
        Alpha (g1) y1b1-
        Player2 (1, r3b2) -b1b1y1y1y3
    )");
    EXPECT_WINNING_MOVE("sacrifice y2; move y1b1 to Player2; catastrophe; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_move_sacrifice_medium_yellow_waypoint) {
    GameState st = from(R"(
        Player1 (0, b3g1) y2g2-
        Alpha (b2) y1-
        Player2 (1, r3b2) -y1y1y3
    )");
    EXPECT_WINNING_MOVE("sacrifice y2; move y1 from Alpha to Waypoint (g1); move y1 to Player2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_no_cat_unneeded_star) {
    GameState st = from(R"(
        Player1 (0, b3g1) b1-
        SacTheMediumBlue (b2) b3b2b1-
        Player2 (1, y3b1) r2r2-b3
    )");
    EXPECT_WINNING_MOVE("sacrifice b2; convert r2 to b2; convert r2 to b2; catastrophe blue");
    EXPECT_LEGAL_MOVE("catastrophe blue");
    EXPECT_ILLEGAL_MOVE("capture b3 at Player2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_precatastrophe_sacrifice_and_move) {
    GameState st = from(R"(
        Player1 (0, r2r3) r1r1r1-
        Alpha (r2) y2-
        Player2 (1, g3) b2-y1y1y1y2g1g1g1b1b1b1
    )");
    EXPECT_ILLEGAL_MOVE("catastrophe green at Player2");
    EXPECT_ILLEGAL_MOVE("catastrophe green; sacrifice r1");
    EXPECT_WINNING_MOVE("catastrophe green; sacrifice y2; move r1r1 to Elsewhere (y1)");
    EXPECT_WINNING_MOVE("catastrophe green; sacrifice y2; move r1r1 to Elsewhere (g1)");
    EXPECT_WINNING_MOVE("catastrophe green; sacrifice y2; move r1r1 to Elsewhere (b1)");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_precatastrophe_sacrifice_and_postcatastrophe) {
    GameState st = from(R"(
        Player1 (0, g2r3) r1r1r1r2r2-
        Alpha (r2) y2-
        Beta (g2) -b1b1b1
        Player2 (1, g3) g2-y1y1y1y2g1g1g1
    )");
    EXPECT_ILLEGAL_MOVE("sacrifice y2 at Alpha; move b2 from Player2 to Waypoint (y1); move b2 from Waypoint to Player1; catastrophe green at Player2");
    EXPECT_ILLEGAL_MOVE("catastrophe green");
    EXPECT_WINNING_MOVE("catastrophe yellow; sacrifice y2; move g2 to Waypoint (y1); move g2 to Player1; catastrophe green");
    EXPECT_WINNING_MOVE("catastrophe yellow; sacrifice y2; move g2 to Waypoint (y1); move g2 to Player1; catastrophe green; catastrophe red");
    EXPECT_LEGAL_MOVE("catastrophe yellow; sacrifice y2; move g2 to Waypoint (y1); move g2 to Player1; catastrophe red");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_precatastrophe) {
    GameState st = from(R"(
        Player1 (0, b3y1) b1b1b1b2-
        Alpha (y3) -g2g2g2
        Beta (y1) -r2r2r2
        Gamma (y1) -r1r1r1g1g1g1
        Player2 (1, b3) b2b2b3-g3y2y2y2
    )");
    EXPECT_ILLEGAL_MOVE("catastrophe blue at Player2");
    EXPECT_ILLEGAL_MOVE("catastrophe blue at Player2; sacrifice b1");
    EXPECT_WINNING_MOVE("catastrophe blue at Player2; sacrifice b1; convert b2 to y2");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_swap_ships) {
    GameState st = from(R"(
        Player1 (0, b2g1) r1y1b2-
        Alpha (b1) -r1y1y1b1
        Player2 (1, y3g1) r1b1-y3y2
    )");
    EXPECT_ILLEGAL_MOVE("convert r1 to y1 at Player2; catastrophe yellow at Player2");
    EXPECT_WINNING_MOVE("sacrifice; convert y1 to g1 at Player1; convert r1 to y1 at Player2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_temporarily_abandon_homeworld) {
    GameState st = from(R"(
        Player1 (0, g3y1) y2-r1
        Alpha (b2) g1-
        Beta (b1) g1-
        Player2 (1, g3y2) -g3g1
    )");
    EXPECT_WINNING_MOVE("sacrifice y2; move g1 to Player1; move g1 to Player2; catastrophe");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_by_destroy_part_of_homeworld) {
    GameState st = from(R"(
        Player1 (0, g3b1) g1g1g1-b1b1
        Alpha (b2) y3y1y1-
        Beta (b2) -r1r1r1
        Player2 (1, r2b3) -y2y1y2
    )");
    EXPECT_WINNING_MOVE("catastrophe; sacrifice y3; move y1 to Player1; move y1 to Waypoint (g1); move y1 to Player2; catastrophe yellow");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_trivial_catastrophe_ships) {
    GameState st = from(R"(
        Player1 (0, y3g2) g1-
        Player2 (1, g3b2) r1y1b1-b1b1
    )");
    EXPECT_WINNING_MOVE("catastrophe blue");
    EXPECT_LEGAL_MOVE("capture b1");
    EXPECT_LEGAL_MOVE("build b2");
    EXPECT_LEGAL_MOVE("pass");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}

TEST(AllMoves, win_trivial_catastrophe_star) {
    GameState st = from(R"(
        Player1 (0, y3g2) g1-
        Player2 (1, r3b2) r1-r2r2b1b1b1y1g1
    )");
    EXPECT_WINNING_MOVE("catastrophe red; catastrophe blue");
    EXPECT_LEGAL_MOVE("capture y1");
    EXPECT_LEGAL_MOVE("capture y1; catastrophe red");
    EXPECT_LEGAL_MOVE("capture y1; catastrophe blue");
    EXPECT_WINNING_MOVE("capture y1; catastrophe red; catastrophe blue");
    EXPECT_WINNING_MOVE("capture g1; catastrophe red; catastrophe blue");
    EXPECT_WINNING_MOVE("capture b1; catastrophe red; catastrophe blue");
    EXPECT_TRUE(findWinningMove(st, 0, nullptr));
}
