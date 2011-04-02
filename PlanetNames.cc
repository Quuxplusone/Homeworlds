
#include <assert.h>
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
            if (actjon.kind != PASS && actjon.where == old_name)
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

