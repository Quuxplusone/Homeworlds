
#include "ApplyMove.h"
#include "Retrograde.h"
#include "WholeMove.h"
#include <gtest/gtest.h>

namespace {

struct Expectation {
    explicit Expectation(int l, bool e, const char *statetext) : line(l), expected(e) {
        GameState st;
        if (!st.scan(statetext)) throw "oops";
        canon = st.toComparableString();
    }
    std::string canon;
    int line;
    bool expected;
    bool found = false;
};

struct Scenario {
    explicit Scenario(const char *statetext) {
        if (!st.scan(statetext)) throw "oops";
        canon = st.toComparableString();
    }
    void expect_predecessor(int line, const char *statetext) {
        data.emplace_back(line, true, statetext);
    }
    void expect_no_predecessor(int line, const char *statetext) {
        data.emplace_back(line, false, statetext);
    }
    ~Scenario() {
        bool bailed = findAllRetrograde(
            st, 0,
            permit_overpopulations,
            permit_postcatastrophes,
            these_colors_only,
            [&](const WholeMove& m, const GameState& oldst) {
                //static int x = 0;
                //putchar(++x % 100 ? '.' : '|'); fflush(stdout);
                std::string this_repr = oldst.toComparableString();
                for (auto& e : data) {
                    if (this_repr == e.canon) {
                        EXPECT_FALSE(e.found);  // This state shouldn't have been found already.
                        e.found = true;
                        EXPECT_TRUE(e.expected);
                        // Sanity-check that the move is applicable.
                        GameState newst2 = oldst;
                        EXPECT_EQ(ApplyMove::Whole(newst2, 0, m), ApplyMove::Result::SUCCESS);
                        EXPECT_EQ(newst2.toComparableString(), canon);
                    }
                }
                return false;
            }
        );
        EXPECT_FALSE(bailed);
        for (const auto& e : data) {
            if (e.expected != e.found) {
                EXPECT_EQ(e.line, "Expectation wasn't met"[22]);
            }
        }
    }

    GameState st;  // the target/final/successor state
    std::string canon;
    std::vector<Expectation> data;
    bool permit_overpopulations = false;
    bool permit_postcatastrophes = false;
    unsigned these_colors_only = 0xF;
};

} // anonymous namespace

static GameState from(const char *text)
{
    GameState st;
    EXPECT_TRUE(st.scan(text));
    return st;
}

TEST(Retrograde, mercury_in_retrograde) {
    GameState st = from(R"(
        Lee (0,g2b1) y3-
        Ray (1,r3b2) g1-r1y3g3
        DS1 (g2) r2g3-r2y1
        DS2 (y2) y3b1b3-
        DS3 (b2) -r3y1y2g3
        DS4 (y2) g1b1-b3
        DS5 (b2) r1-
        DS6 (g2) r1b3-
        DS7 (g1) -r2r3y1
    )");
    std::vector<WholeMove> moves;
    findAllRetrograde(
        st, 0,
        true, true, 0xF,
        [&](const WholeMove& m, const GameState&) {
            moves.push_back(m);
            return false;
        }
    );
    EXPECT_EQ(moves.size(), 1u);
    EXPECT_EQ(moves[0].toString(), "move y3 from DS7 to DS2");
}

TEST(Retrograde, capture_simple) {
    Scenario s(R"(
        Player1 (0, r3) g3g1-
        Player2 (1, r3) -b3
    )");
    s.these_colors_only = (1u << RED);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g3-g1
        Player2 (1, r3) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) g1-g3
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_capture_of_bigger_ship) {
    Scenario s(R"(
        Player1 (0, g3) r1y2g3-
        DS1 (g3) r2-
        Player2 (1, r3) -b3
    )");
    s.these_colors_only = (1u << RED);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) r1g3-y2
        DS1 (g3) r2-
        Player2 (1, r3) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) y2g3-r1
        DS1 (g3) r2-
        Player2 (1, r3) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) r1y2g3-
        DS1 (g3) -r2
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_capture_without_red_access) {
    Scenario s(R"(
        Player1 (0, g3) y2g3-
        DS1 (g3) b3-
        Player2 (1, r3) -b3
    )");
    s.these_colors_only = (1u << RED);
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-y2
        DS1 (g3) b3-
        Player2 (1, r3) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) g3y2-
        DS1 (g3) -b3
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, move_simple) {
    Scenario s(R"(
        Player1 (0, g3) y2g3-
        DS1 (g2) b3-
        Player2 (1, y1) -b3
    )");
    s.these_colors_only = (1u << YELLOW);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS1 (g2) y2b3-
        Player2 (1, y1) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        Player2 (1, y1) y2-b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        Player2 (1, y1) g3-b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) y2-
        DS1 (g2) g3b3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, discover_simple) {
    Scenario s(R"(
        Player1 (0, g3) y2g3-
        DS1 (g2) b3-
        DS2 (r1) g1-
        DS3 (y2) y1-
        Player2 (1, y1) -b3
    )");
    s.these_colors_only = (1u << YELLOW);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) y2g3b3-
        DS2 (r1) g1-
        DS3 (y2) y1-
        Player2 (1, y1) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) y2g3-
        DS2 (r1) g1-
        DS3 (y2) y1-
        Player2 (1, y1) b3-b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) y2g3-
        DS2 (r1) g1b3-
        DS3 (y2) y1-
        Player2 (1, y1) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) y2g3-
        DS2 (r1) g1-
        DS3 (y2) y1b3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, move_with_dismantling) {
    Scenario s(R"(
        Player1 (0, g3) y2g3-
        DS1 (g2) b3-
        Player2 (1, y1) -b3
    )");
    s.these_colors_only = (1u << YELLOW);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        DS2 (g2) y2-
        Player2 (1, y1) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        DS2 (y1) y2-
        Player2 (1, y1) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        DS2 (y1) g3-
        Player2 (1, y1) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        DS2 (y3) y2-
        Player2 (1, y1) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        DS2 (y3) g3-
        Player2 (1, y1) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) y2-
        DS1 (g2) b3-
        DS2 (b1) g3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, discover_with_dismantling) {
    Scenario s(R"(
        Player1 (0, g3) g3-
        DS1 (g2) b3-
        Player2 (1, y1) -b3
    )");
    s.these_colors_only = (1u << YELLOW);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS2 (y1) b3-
        Player2 (1, y1) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS2 (y3) b3-
        Player2 (1, y1) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS2 (y2) b3-
        Player2 (1, y1) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) g3-
        DS2 (b3) b3-
        Player2 (1, y1) -b3
    )");
}

TEST(Retrograde, build_simple) {
    Scenario s(R"(
        Player1 (0, r3) g2g1b2b1y1-
        Player2 (1, r3b1) -b1
    )");
    s.these_colors_only = (1u << GREEN);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2b2b1y1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2g1b1y1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2g1b2y1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) g1b2b1y1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) g2g1b2b1-
        Player2 (1, r3b1) -b1
    )");
}

TEST(Retrograde, move_away_from_overpopulation) {
    Scenario s(R"(
        Player1 (0, g3) y1b1-r2
        DS1 (0, y2) y1-
        DS2 (0, b3) y1-
        Player2 (1, r3) -b3
    )");
    s.these_colors_only = (1u << YELLOW);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1-r2
        DS1 (0, y2) y1y1y2-
        DS2 (0, b3) y1-
        Player2 (1, r3) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1-r2
        DS1 (0, y2) y1-
        DS2 (0, b3) y1y1y2-
        Player2 (1, r3) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1-r2
        DS1 (0, y2) y1-
        DS2 (0, b3) y1-
        DS3 (0, g2) y1y2y2y3y3-
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_build_without_green_access) {
    Scenario s(R"(
        Player1 (0, r3) b2b1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) b2-
        Player2 (1, r3b1) -b1
    )");
}

TEST(Retrograde, trade_away_only_blue) {
    Scenario s(R"(
        Player1 (0, g3) g1-
        Player2 (1, r3) -b3
    )");
    s.these_colors_only = (1u << BLUE);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1-
        Player2 (1, r3) -b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, g3) r1-
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, trade_away_from_overpopulation) {
    Scenario s(R"(
        Player1 (0, g3) b1b1-g1g1
        Player2 (1, r3) -b3
    )");
    s.permit_overpopulations = true;
    s.these_colors_only = (1u << BLUE);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1r1-g1g1
        Player2 (1, r3) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1y1-g1g1
        Player2 (1, r3) -b3
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, g3) b1g1-g1g1
        Player2 (1, r3) -b3
    )");
}

TEST(Retrograde, impossible_trade_without_blue_access) {
    Scenario s(R"(
        Player1 (0, r3) g2g1-
        Player2 (1, r3b1) -b1
    )");
    s.these_colors_only = (1u << BLUE);
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) y2g1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) b2g1-
        Player2 (1, r3b1) -b1
    )");
}

TEST(Retrograde, free_move_to_catastrophe) {
    Scenario s(R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
    )");
#if 0  // TODO FIXME BUG HACK: this is much too slow
    s.permit_postcatastrophes = true;
    s.these_colors_only = (1u << YELLOW);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) b3-r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b1) -b3b3y1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b1) -b3b3
        DS4 (y1) b3-
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b1) -b3b3
        DS4 (r3) b3y1-
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b1) -b3b3
        DS4 (r3) b3-y1
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b1) -b3b3
        DS4 (r1) b3y1-
    )");
#endif
}

TEST(Retrograde, free_build_to_catastrophe) {
    Scenario s(R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
    )");
#if 0  // TODO FIXME BUG HACK: this is much too slow
    s.permit_postcatastrophes = true;
    s.these_colors_only = (1u << GREEN);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b3) g1b1-y1b3
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3b2) r3-g3g3
        Player2 (1, g3b2) -r2r3y2y3y3g2g2
        DS1 (r2) y2-
        DS2 (b2) -r2y2y3g2
        DS3 (r1) -r1y1y1g1g1b1b1
        Boom (b3) b1-y1b3
    )");
#endif
}

TEST(Retrograde, free_trade_to_catastrophe) {
    Scenario s(R"(
        Player1 (0, r3) g2-
        Player2 (1, r3b1) -b1
    )");
#if 0  // TODO FIXME BUG HACK: this is much too slow
    s.permit_postcatastrophes = true;
    s.these_colors_only = (1u << BLUE);
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2-
        Player1 (y1) r3b1-y2y2y2
        Player2 (1, r3b1) -b1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2-
        Player1 (r1) b1y2-y2y2
        Player2 (1, r3b1) -b1
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) g2-
        Player1 (r1) b2y2-y2y2
        Player2 (1, r3b1) -b1
    )");
#endif
}

TEST(Retrograde, pointless_sacrifice) {
    Scenario s(R"(
        Player1 (0, r3) g2-
        Player2 (1, r3b1) -b1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2-
        Player1 (y1) r3-
        Player2 (1, r3b1) -b1
    )");
    s.expect_predecessor(__LINE__, R"(
        Player1 (0, r3) g2-
        Player1 (b3) b1-
        Player2 (1, r3b1) -b1
    )");
    s.expect_no_predecessor(__LINE__, R"(
        Player1 (0, r3) g2-
        Player1 (b3) r3r1-
        Player2 (1, r3b1) -b1
    )");
}
