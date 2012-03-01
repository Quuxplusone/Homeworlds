
#include <assert.h>
#include <string>
#include <vector>
#include "move.h"
#include "state.h"
#include "PlanetNames.h"

static const char *default_names[21] = {
    "Delos", "Lebling", "Kotok", "Minsky", "Licklider", "Fritz", "Frotz",
    "Shrdlu", "Lighthill", "Crowther", "Plugh", "Calvin", "Winograd", "Garner",
    "Weizenbaum", "Knuth", "Gibson", "Stuyvesant", "Prufrock", "Marlow", "Daisy"
};

void reassignPlanetNames(WholeMove &move, const GameState &st, const char *names[21])
{
    int new_name_index = 0;
    if (names == NULL)
      names = default_names;
    for (int i=0; i < (int)move.actions.size(); ++i) {
        SingleAction &action = move.actions[i];
        if (action.kind != MOVE_CREATE) continue;
        assert(action.whither != "");
        /* We need to pick a new name for this star system. */
        const char *new_name = NULL;
        for ( ; new_name_index < 21; ++new_name_index) {
            assert(names[new_name_index] != NULL);
            assert(StarSystem::is_valid_name(names[new_name_index]));
            if (st.systemNamed(names[new_name_index]) == NULL) {
                new_name = names[new_name_index];
                ++new_name_index;
                break;
            }
        }
        /* Since there can be only 18 systems at one time, and only 3 new
         * systems can be created in one complete move, it follows that we
         * must have found one of our 21 system names not being used. */
        assert(new_name != NULL);
        const std::string &old_name = action.whither;
        for (int j=i+1; j < (int)move.actions.size(); ++j) {
            SingleAction &actjon = move.actions[j];
            if (actjon.where == old_name)
              move.actions[j].where = new_name;
            if (actjon.kind == MOVE && actjon.whither == old_name)
              actjon.whither = new_name;
        }
        action.whither = new_name;
    }
}


void assignPlanetNames(GameState &st, const char *names[21])
{
    int new_name_index = 0;
    if (names == NULL)
      names = default_names;
    for (int i=0; i < (int)st.stars.size(); ++i) {
        if (st.stars[i].name != "") continue;
        /* We need to pick a new name for this star system. */
        const char *new_name = NULL;
        for ( ; new_name_index < 21; ++new_name_index) {
            assert(names[new_name_index] != NULL);
            assert(StarSystem::is_valid_name(names[new_name_index]));
            if (st.systemNamed(names[new_name_index]) == NULL) {
                new_name = names[new_name_index];
                ++new_name_index;
                break;
            }
        }
        /* Since there can be only 18 systems at one time, and only 3 new
         * systems can be created in one complete move, it follows that we
         * must have found one of our 21 system names not being used. */
        assert(new_name != NULL);
        st.stars[i].name = new_name;
    }
}


void reassignNamesToMove(WholeMove &move, const GameState &st, const GameState &st2)
{
    assert(st.toComparableString() == st2.toComparableString());
    int n = st.stars.size();
    assert(n == (int)st2.stars.size());
    std::vector<std::string> mapping(n);  /* what's the name of st.stars[i] in st2? */
    std::vector<bool> mapped(n);  /* is st2.stars[i] already listed in "mapping"? */
    for (int i=0; i < n; ++i) {
        const StarSystem &sys1 = st.stars[i];
        if (sys1.homeworldOf >= 0) {
            const StarSystem *sys2 = st2.homeworldOf(sys1.homeworldOf);
            assert(sys2 != NULL);
            mapping[i] = sys2->name;
            mapped[sys2 - &st2.stars[0]] = true;
        } else {
            mapping[i] = "-";
            std::string sys1_str = sys1.toComparableString();
            for (int j=0; j < n; ++j) {
                const StarSystem *sys2 = &st2.stars[j];
                if (mapped[j]) continue;
                if (sys2->toComparableString() != sys1_str) continue;
                mapping[i] = sys2->name;
                mapped[sys2 - &st2.stars[0]] = true;
                break;
            }
            assert(mapping[i] != "-");
        }
    }
    
    for (int i=0; i < (int)move.actions.size(); ++i) {
        SingleAction &action = move.actions[i];
        const StarSystem *sys1 = st.systemNamed(action.where.c_str());
        assert(sys1 != NULL);
        action.where = mapping[sys1 - &st.stars[0]];
        assert(st2.systemNamed(action.where.c_str()) != NULL);
        if (action.kind == MOVE) {
            const StarSystem *sys1b = st.systemNamed(action.whither.c_str());
            assert(sys1b != NULL);
            action.whither = mapping[sys1b - &st.stars[0]];
            assert(st2.systemNamed(action.whither.c_str()) != NULL);
        }
    }
}

