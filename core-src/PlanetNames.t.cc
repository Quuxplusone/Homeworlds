
#include "PlanetNames.h"
#include <gtest/gtest.h>

static GameState from(const char *text)
{
    GameState st;
    EXPECT_TRUE(st.scan(text));
    return st;
}

TEST(PlanetNames, assignPlanetNamesToEmpty) {
    GameState st;
    assignPlanetNames(&st);
    EXPECT_EQ(st.toString(), "");
}

TEST(PlanetNames, assignPlanetNamesToSetup) {
    GameState st = from(R"(
        (0, r3g1) b3-
    )");
    assignPlanetNames(&st);
    EXPECT_EQ(st.toString(), "Delos (0, r3g1) b3-\n");
}

TEST(PlanetNames, assignPlanetNamesToSetupWithCustomList) {
    GameState st = from(R"(
        (0, r3g1) b3-
    )");
    const char *halyul[3] = { "Hal", "Yul", nullptr };
    assignPlanetNames(&st, halyul);
    EXPECT_EQ(st.toString(), "Hal (0, r3g1) b3-\n");
}

TEST(PlanetNames, assignPlanetNamesToAll) {
    GameState st = from(R"(
        (0, r3g1) b3-
        (b3) g1g2-
        (1, y2g1) -b3
    )");
    assignPlanetNames(&st);
    EXPECT_EQ(st.toString(), "Delos (0, r3g1) b3-\nLebling (b3) g1g2-\nKotok (1, y2g1) -b3\n");
}

TEST(PlanetNames, assignPlanetNamesToSome) {
    GameState st = from(R"(
        Sam (0, r3g1) b3-
        (b3) g1g2-
        Dave (1, y2g1) -b3
    )");
    assignPlanetNames(&st);
    EXPECT_EQ(st.toString(), "Sam (0, r3g1) b3-\nDelos (b3) g1g2-\nDave (1, y2g1) -b3\n");
}

TEST(PlanetNames, assignPlanetNamesWorstCase) {
    GameState st = from(R"(
        (0, r1) r1-
        (r1) r2-
        (r2) r2-
        (r3) r3-
        (r3) y1-
        (y1) y1-
        (y2) y2-
        (y2) y3-
        (y3) y3-
        (g1) g1-
        (g1) g2-
        (g2) g2-
        (g3) g3-
        (g3) b1-
        (b1) b1-
        (b2) b2-
        (b2) b3-
        (1, b3) -b3
    )");
    assignPlanetNames(&st);
    EXPECT_EQ(
        st.toString(),
        "Delos (0, r1) r1-\nLebling (r1) r2-\nKotok (r2) r2-\nMinsky (r3) r3-\n"
        "Licklider (r3) y1-\nFritz (y1) y1-\nFrotz (y2) y2-\nShrdlu (y2) y3-\n"
        "Lighthill (y3) y3-\nCrowther (g1) g1-\nPlugh (g1) g2-\nCalvin (g2) g2-\n"
        "Winograd (g3) g3-\nGarner (g3) b1-\nWeizenbaum (b1) b1-\nKnuth (b2) b2-\n"
        "Gibson (b2) b3-\nStuyvesant (1, b3) -b3\n"
    );
}

TEST(PlanetNames, reassignPlanetNames) {
    GameState st = from(R"(
        Sam (0, r3g1) b3y2-
        Alpha (b3) g1g2-
        Dave (1, y2g1) -b3
    )");
    WholeMove m("move y2 from Sam to Dummy (b2)");
    reassignPlanetNames(&m, st);
    EXPECT_EQ(m.toString(), "move y2 from Sam to Delos (b2)");
}

TEST(PlanetNames, reassignPlanetNamesWithHomeworldMove) {
    GameState st = from(R"(
        Sam (0, r3g1) b3y2-
    )");
    WholeMove m("homeworld y2 g1 b3 Dave");
    reassignPlanetNames(&m, st);
    EXPECT_EQ(m.toString(), "homeworld y2 g1 b3 Delos");
}

TEST(PlanetNames, reassignPlanetNamesWithHwAndCustomList) {
    GameState st = from(R"(
        Sam (0, r3g1) b3y2-
    )");
    WholeMove m("homeworld y2 g1 b3 Dave");
    const char *halyul[3] = { "Hal", "Yul", nullptr };
    reassignPlanetNames(&m, st, halyul);
    EXPECT_EQ(m.toString(), "homeworld y2 g1 b3 Hal");
}

TEST(PlanetNames, reassignPlanetNamesWithHwAndCustomListAndExternalConflict) {
    GameState st = from(R"(
        Hal (0, r3g1) b3y2-
    )");
    WholeMove m("homeworld y2 g1 b3 Dave");
    const char *halyul[3] = { "Hal", "Yul", nullptr };
    reassignPlanetNames(&m, st, halyul);
    EXPECT_EQ(m.toString(), "homeworld y2 g1 b3 Yul");
}

TEST(PlanetNames, reassignPlanetNamesWithExternalConflict) {
    GameState st = from(R"(
        Sam (0, r3g1) b3y2-
        Alpha (b3) g1g2-
        Delos (b1) -g2y3
        Lebling (r1) -y1
        Dave (1, y2g1) -b3
    )");
    WholeMove m("move y2 from Sam to Dummy (b2)");
    reassignPlanetNames(&m, st);
    EXPECT_EQ(m.toString(), "move y2 from Sam to Kotok (b2)");
}

TEST(PlanetNames, reassignPlanetNamesWithInternalConflict) {
    GameState st = from(R"(
        Sam (0, r3g1) b3y2-
        Alpha (b3) g1g2-
        Delos (b1) -g2y3
        Lebling (r1) -y1
        Dave (1, y2g1) -b3
    )");
    WholeMove m("sac y2 Sam; move g2 from Alpha to Dummy (b2); move g1 from Alpha to Kotok (b2)");
    reassignPlanetNames(&m, st);
    EXPECT_EQ(m.toString(), "sacrifice y2 at Sam; move g2 from Alpha to Kotok (b2); move g1 from Alpha to Minsky (b2)");
}

TEST(PlanetNames, reassignPlanetNamesWithInternalRepetition) {
    GameState st = from(R"(
        Sam (0, r3g1) b3y2-
        Alpha (b3) g1g2-
        Delos (b1) -g2y3
        Lebling (r1) -y1
        Dave (1, y2g1) -b3
    )");
    WholeMove m("sac y2 Sam; move g2 from Alpha to Dummy (b2); move g1 from Alpha to Dummy");
    reassignPlanetNames(&m, st);
    EXPECT_EQ(m.toString(), "sacrifice y2 at Sam; move g2 from Alpha to Kotok (b2); move g1 from Alpha to Kotok");
}

TEST(PlanetNames, reassignPlanetNamesWorstCase) {
    GameState st = from(R"(
        Delos (0, r1) r1-
        Lebling (r1) r2-
        Kotok (r2) r2-
        Minsky (r3) r3-
        Licklider (r3) y1-
        Fritz (y1) y1-
        Frotz (y2) y2-
        Shrdlu (y2) y3-
        Lighthill (y3) y3-
        Crowther (g1) g1-
        Plugh (g1) g2-
        Calvin (g2) g2-
        Winograd (g3) g3-
        Garner (g3) b1-
        Weizenbaum (b1) b1-
        Knuth (b2) b2-
        Gibson (b2) b3-
        Stuyvesant (1, b3) -b3
    )");
    WholeMove m("sacrifice y3 Shrdlu; discover r2 Lebling y3 X; discover r2 Kotok r1 Y; discover r3 Minsky r2 Z");
    reassignPlanetNames(&m, st);
    EXPECT_EQ(
        m.toSDGString(),
        "sacrifice y3 Shrdlu; discover r2 Lebling y3 Prufrock; discover r2 Kotok r1 Marlow; discover r3 Minsky r2 Daisy"
    );
}
