
#include <assert.h>
#include <string.h>
#include <string>
#include <vector>
#include "state.h"
#include "PlanetNames.h"
#include "SingleAction.h"
#include "WholeMove.h"

static const char *default_names[21] = {
    "Delos", "Lebling", "Kotok", "Minsky", "Licklider", "Fritz", "Frotz",
    "Shrdlu", "Lighthill", "Crowther", "Plugh", "Calvin", "Winograd", "Garner",
    "Weizenbaum", "Knuth", "Gibson", "Stuyvesant", "Prufrock", "Marlow", "Daisy"
};

static auto get_new_name_lambda(const char *names[21])
{
    return [names, i=0](const GameState& st) mutable -> const char * {
        for ( ; i < 21; ++i) {
            const char *new_name = names[i];
            assert(new_name != nullptr);
            assert(StarSystem::isValidName(new_name));
            if (st.systemNamed(new_name) == nullptr) {
                ++i;
                return new_name;
            }
        }
        /* Since there can be only 18 systems at one time, and only 3 new
         * systems can be created in one complete move, it follows that we
         * must have found one of our 21 system names not being used. */
        assert(false);
    };
}

void reassignPlanetNames(WholeMove *move, const GameState& st)
{
    reassignPlanetNames(move, st, default_names);
}

void reassignPlanetNames(WholeMove *move, const GameState& st, const char *names[21])
{
    assert(move != nullptr);
    assert(names != nullptr);

    auto get_new_name = get_new_name_lambda(names);

    for (int i=0; i < (int)move->actions.size(); ++i) {
        SingleAction& action = move->actions[i];
        if (action.kind == HOMEWORLD) {
            assert(action.where != "");
            /* We need to pick a new name for this star system. */
            action.where = get_new_name(st);
        } else if (action.kind == MOVE_CREATE) {
            assert(action.whither != "");
            /* We need to pick a new name for this star system. */
            const char *new_name = get_new_name(st);
            const std::string& old_name = action.whither;
            for (int j=i+1; j < (int)move->actions.size(); ++j) {
                SingleAction& actjon = move->actions[j];
                if (actjon.where == old_name) {
                    actjon.where = new_name;
                }
                if (actjon.kind == MOVE && actjon.whither == old_name) {
                    actjon.whither = new_name;
                }
            }
            action.whither = new_name;
        }
    }
}

void assignPlanetNames(GameState *st)
{
    assignPlanetNames(st, default_names);
}

void assignPlanetNames(GameState *st, const char *names[21])
{
    assert(st != nullptr);
    assert(names != nullptr);

    auto get_new_name = get_new_name_lambda(names);

    for (StarSystem& star : st->stars) {
        if (star.name == "") {
            /* We need to pick a new name for this star system. */
            star.name = get_new_name(*st);
        }
    }
}

void reassignNamesToMove(WholeMove *move, const GameState& st, const GameState& st2)
{
    assert(move != nullptr);
    assert(st.toComparableString() == st2.toComparableString());
    int n = st.stars.size();
    assert(n == (int)st2.stars.size());
    std::vector<std::string> mapping(n);  /* what's the name of st.stars[i] in st2? */
    std::vector<bool> mapped(n);  /* is st2.stars[i] already listed in "mapping"? */
    for (int i=0; i < n; ++i) {
        const StarSystem& sys1 = st.stars[i];
        if (sys1.homeworldOf >= 0) {
            const StarSystem *sys2 = st2.homeworldOf(sys1.homeworldOf);
            assert(sys2 != nullptr);
            mapping[i] = sys2->name;
            mapped[sys2 - &st2.stars[0]] = true;
        } else {
            mapping[i] = "-";
            char sys1_str[StarSystem::MAXSTRLEN+1];
            *sys1.toComparableString(sys1_str) = '\0';
            for (int j=0; j < n; ++j) {
                const StarSystem *sys2 = &st2.stars[j];
                if (mapped[j]) continue;
                char sys2_str[StarSystem::MAXSTRLEN+1];
                *sys2->toComparableString(sys1_str) = '\0';
                if (strcmp(sys2_str, sys1_str) != 0) continue;
                mapping[i] = sys2->name;
                mapped[sys2 - &st2.stars[0]] = true;
                break;
            }
            assert(mapping[i] != "-");
        }
    }

    for (SingleAction& action : move->actions) {
        const StarSystem *sys1 = st.systemNamed(action.where.c_str());
        assert(sys1 != nullptr);
        action.where = mapping[sys1 - &st.stars[0]];
        assert(st2.systemNamed(action.where.c_str()) != nullptr);
        if (action.kind == MOVE) {
            const StarSystem *sys1b = st.systemNamed(action.whither.c_str());
            assert(sys1b != nullptr);
            action.whither = mapping[sys1b - &st.stars[0]];
            assert(st2.systemNamed(action.whither.c_str()) != nullptr);
        }
    }
}
