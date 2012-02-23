
#include "state.h"
#include "wxstuff.h"
#include <string>
#include <vector>

GalaxyWidget::GalaxyWidget(wxWindow *p, int id) :
    wxWindow(p, id, wxDefaultPosition, wxDefaultSize, wxBORDER_SUNKEN),
    attacker_homeworld(NULL),
    defender_homeworld(NULL),
    num_systems(0)
{
    this->SetMinSize(wxSize(500,400));
    this->SetBackgroundStyle(wxBG_STYLE_COLOUR);
    this->SetBackgroundColour(wxColour(0,0,0));
    this->SetSizer(new wxBoxSizer(wxVERTICAL));
    this->Layout();
}

SystemWidget *GalaxyWidget::nth_system(int i)
{
    assert(0 <= i && i < this->num_systems);
    wxSizerItem *it = this->GetSizer()->GetItem(i);
    assert(it != NULL);
    SystemWidget *sw = (SystemWidget *)it->GetWindow();
    assert(sw != NULL);
    return sw;
}


SystemWidget *GalaxyWidget::add_system(const wxPoint &pos, const std::string &name)
{
    SystemWidget *np = new SystemWidget(this, wxID_ANY, pos);
    np->name = name;
    this->GetSizer()->Add(np, 0, wxEXPAND | wxALL, /*border=*/5);
    ++num_systems;
    return np;
}

void GalaxyWidget::delete_system(SystemWidget *sw)
{
    if (sw == this->attacker_homeworld || sw == this->defender_homeworld) {
	sw->star = NULL;
	sw->GetSizer()->Clear(true);
	sw->Layout();
    } else {
	this->GetSizer()->Detach(sw);
	sw->Destroy();
	--num_systems;
	this->Layout();
    }
}

void GalaxyWidget::restash_and_delete_system(SystemWidget *sw)
{
    assert(sw != NULL);
    assert(this->stash != NULL);
    /* First, put all the affected pieces back into the stash. */
    for (int i=0; true; ++i) {
	wxSizerItem *it = sw->GetSizer()->GetItem(i);
	if (it == NULL) break;
	PieceWidget *pw = (PieceWidget *)it->GetWindow();
	assert(pw != NULL);
	this->stash->add_piece(pw->piece_size, pw->piece_color);
	if (pw->second_color != UNKNOWN_COLOR) {
	    this->stash->add_piece(pw->second_size, pw->second_color);
	}
    }
    /* Now remove the system from the map. */
    this->delete_system(sw);
    this->stash->Refresh();
}

std::string GalaxyWidget::get_new_system_name()
{
    const char *names[40] = {
	"Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta",
	"Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi",
	"Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega",
	"Aleph", "Beth", "Gimel", "Daleth", "He", "Waw", "Zayin", "Heth",
	"Teth", "Yod", "Kaph", "Lamed", "Mem", "Nun", "Samekh", "Ayin"
    };
    for (int i = 0; i < 40; ++i) {
	const char *result = names[i];
	if (attacker_homeworld != NULL && result == attacker_homeworld->name)
	    continue;
	if (defender_homeworld != NULL && result == defender_homeworld->name)
	    continue;
	bool okay = true;
	for (int j=0; j < num_systems; ++j) {
	    if (result == nth_system(j)->name) {
		okay = false;
		break;
	    }
	}
	if (okay)
	    return result;
    }
    /* With 40 names to choose from and only 36 pieces in the game, we should
     * really never run out of names. */
    assert(false);
    return "Foo";
}

void GalaxyWidget::update(const GameState &st)
{
    assert(attacker_homeworld != NULL);
    assert(defender_homeworld != NULL);
    assert(stash != NULL);
    for (int i=0; i < (int)st.stars.size(); ++i) {
	const StarSystem &star = st.stars[i];
	if (star.homeworldOf == 0) {
	    attacker_homeworld->update(&star);
	} else if (star.homeworldOf == 1) {
	    defender_homeworld->update(&star);
	} else {
	    assert(star.homeworldOf == -1);
	    bool found = false;
	    for (int i=0; i < num_systems; ++i) {
		if (nth_system(i)->name == star.name) {
		    nth_system(i)->update(&star);
		    found = true;
		}
	    }
	    if (!found) {
		/* There's a new star in "st" which we don't already have
		 * in our widget.  Add it to the galaxy map. */
		SystemWidget *np = this->add_system(wxDefaultPosition, star.name);
		np->update(&star);
	    }
	}
    }
    /* Now, look for any old stars in our galaxy map which no longer exist
     * in "st". If you find any, then remove them from the galaxy map. */
    for (int i=0; i < num_systems; ++i) {
	SystemWidget *sw = nth_system(i);
	std::string name = sw->name;
	if (st.systemNamed(name.c_str()) == NULL) {
	    /* Remove the system widget from our galaxy map. */
	    this->delete_system(sw);
	    --i;
	}
    }
    if (st.systemNamed(attacker_homeworld->name.c_str()) == NULL) {
	attacker_homeworld->update(NULL);
    }
    if (st.systemNamed(defender_homeworld->name.c_str()) == NULL) {
	defender_homeworld->update(NULL);
    }
    stash->update(st.stash);

    this->Layout();
    this->GetParent()->Layout();
    stash->Layout();
}


/* Helper function for GalaxyWidget::to_state(). Convert a single system
 * to its GameState representation. */
static void add_to_state(GameState &st, SystemWidget *sw, int who)
{
    assert(sw != NULL);
    st.stars.push_back(StarSystem(sw->name.c_str()));
    StarSystem &ss = st.stars.back();
    ss.homeworldOf = who;
    /* Now add the ships. */
    bool contains_ships = false;
    for (int j=0; true; ++j) {
	wxSizerItem *si = sw->GetSizer()->GetItem(j);
	if (si == NULL) break;
	assert(si->IsWindow());
	PieceWidget *pw = (PieceWidget *)si->GetWindow();
	assert(pw != NULL);
	if (pw->whose == -1) {
	    assert(j == 0);
	    assert(pw == sw->star);
	    ss.star.insert(pw->piece_color, pw->piece_size);
	    st.stash.remove(pw->piece_color, pw->piece_size);
	    if (pw->second_color != UNKNOWN_COLOR) {
		ss.star.insert(pw->second_color, pw->second_size);
		st.stash.remove(pw->second_color, pw->second_size);
	    }
	} else {
	    assert(pw->whose == 0 || pw->whose == 1);
	    ss.ships[pw->whose].insert(pw->piece_color, pw->piece_size);
	    st.stash.remove(pw->piece_color, pw->piece_size);
	    contains_ships = true;
	}
    }
    assert((sw->star == NULL) == ss.star.empty());
    if (contains_ships) {
	assert(sw->star != NULL);
    } else if (sw->star != NULL) {
	/* This system has a star and nothing else. This is technically
	 * forbidden by the rules of the game, but GameState should be
	 * able to handle it anyway. */
    } else {
	/* This system is totally empty. The only way this can happen is
	 * if it's a homeworld system, i.e., who != -1. */
	assert(who != -1);
	st.stars.resize(st.stars.size()-1);
    }
}

/* Convert the state represented by this galaxy map back into a GameState. */
GameState GalaxyWidget::to_state()
{
    assert(attacker_homeworld != NULL);
    assert(defender_homeworld != NULL);
    assert(stash != NULL);
    GameState st;
    st.newGame();
    add_to_state(st, attacker_homeworld, 0);
    add_to_state(st, defender_homeworld, 1);
    for (int i=0; i < num_systems; ++i) {
	add_to_state(st, nth_system(i), -1);
    }    
    return st;
}

