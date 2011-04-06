
#include "wxstuff.h"
#include <vector>

DraggableType type_of_thing_being_dragged;
wxWindow *thing_being_dragged = NULL;
int global_attacker = 0;
bool just_started_new_game = false;

struct GameApp
{
    void OnMouseEvent(wxMouseEvent &e);
};


static bool catastrophe_possible(SystemWidget *sw, Color c)
{
    int count = 0;
    wxSizer *gs = sw->GetSizer();
    assert(gs != NULL);
    for (int i=0; true; ++i) {
	wxSizerItem *it = gs->GetItem(i);
	if (it == NULL) break;
	PieceWidget *pw = (PieceWidget *)it->GetWindow();
	assert(pw != NULL);
	if (pw->piece_color == c)
	    ++count;
	if (pw->second_color == c)
	    ++count;
    }
    return (count >= 4);
}

static void catastrophe(SystemWidget *sw, Color c)
{
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    assert(gp->stash != NULL);

    assert(sw->star != NULL);
    const bool everything_must_go =
	(sw->star->piece_color == c &&
	    (sw->star->second_color == c ||
	     sw->star->second_color == UNKNOWN_COLOR));

    std::vector<PieceWidget *> things_to_detach;

    wxSizer *gs = sw->GetSizer();
    assert(gs != NULL);
    for (int i=0; true; ++i) {
	wxSizerItem *it = gs->GetItem(i);
	if (it == NULL) break;
	PieceWidget *pw = (PieceWidget *)it->GetWindow();
	assert(pw != NULL);
	assert(pw->piece_color != UNKNOWN_COLOR);
	if (pw->piece_color == c || pw->second_color == c || everything_must_go) {
	    /* We're getting rid of at least part of this piece. */
	    if (pw->whose == -1) {
		/* A star is involved in a catastrophe. */
		assert(i == 0);
		assert(pw == sw->star);
		if (everything_must_go) {
		    gp->stash->add_piece(pw->piece_size, pw->piece_color);
		    if (pw->second_color != UNKNOWN_COLOR) {
			gp->stash->add_piece(pw->second_size, pw->second_color);
		    }
		} else if (pw->piece_color != c) {
		    /* Just the second piece is being destroyed. */
		    assert(pw->second_color == c);
		    gp->stash->add_piece(pw->second_size, pw->second_color);
		    pw->second_color = UNKNOWN_COLOR;
		    pw->second_size = UNKNOWN_SIZE;
		} else {
		    assert(pw->second_color != c && pw->second_color != UNKNOWN_COLOR);
		    /* Just the first piece is being destroyed. */
                    assert(gp->stash != NULL);
		    gp->stash->add_piece(pw->piece_size, pw->piece_color);
		    pw->piece_color = pw->second_color;
		    pw->piece_size = pw->second_size;
		    pw->second_color = UNKNOWN_COLOR;
		    pw->second_size = UNKNOWN_SIZE;
		}
	    } else {
		gp->stash->add_piece(pw->piece_size, pw->piece_color);
		if (!everything_must_go) {
		    things_to_detach.push_back(pw);
		}
	    }
	}
    }
    if (everything_must_go) {
	assert(things_to_detach.empty());
	gp->delete_system(sw);
    } else {
	/* A catastrophe that doesn't destroy the star must involve at least
	 * three ships. */
	assert(things_to_detach.size() >= 3);
	for (int i=0; i < (int)things_to_detach.size(); ++i) {
	    sw->GetSizer()->Detach(things_to_detach[i]);
	    things_to_detach[i]->Destroy();
	}
        sw->Layout();
    }
}

void double_click_piece(PieceWidget *pw)
{
    SystemWidget *sw = (SystemWidget *)pw->GetParent();
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(sw != NULL);
    if (catastrophe_possible(sw, pw->piece_color)) {
	/* Double-clicking a piece that's part of an overpopulation
	 * causes a catastrophe. */
	catastrophe(sw, pw->piece_color);
	gp->Layout();
	gp->stash->Layout();
    } else if (pw->second_color != UNKNOWN_COLOR &&
	    catastrophe_possible(sw, pw->second_color)) {
	catastrophe(sw, pw->second_color);
	gp->Layout();
	gp->stash->Layout();
    } else if (pw->whose != -1) {
	/* Double-clicking an enemy ship should capture it.
	 * Double-clicking a friendly ship should un-capture it (which is
	 * useful if you double-clicked to capture the wrong ship by mistake;
	 * then you don't have to reset the whole board). */
	pw->whose = (1 - pw->whose);
	sw->Layout();
    } else {
	/* Double-clicking a non-overpopulated star doesn't do anything. */
    }
}


static void event_within_system(wxMouseEvent &e, const wxPoint &clickpos, SystemWidget *sw)
{
    wxSizer *gs = sw->GetSizer();
    assert(gs != NULL);
    for (int i=0; true; ++i) {
	wxSizerItem *it = gs->GetItem(i);
	if (it == NULL) break;
	PieceWidget *pw = (PieceWidget *)it->GetWindow();
	assert(pw != NULL);
	if (!pw->GetScreenRect().Contains(clickpos))
	    continue;
	if (e.LeftDClick()) {
	    double_click_piece(pw);
	    thing_being_dragged = NULL;
	} else if (e.LeftUp() && thing_being_dragged != NULL) {
	    pw->mouseup();
	} else if (e.LeftDown()) {
	    assert(thing_being_dragged == NULL);
	    type_of_thing_being_dragged = DT_PIECE;
	    thing_being_dragged = pw;
	}
	return;
    }
    /* Event was within an empty area of this SystemWidget. */
    if (e.LeftUp() && thing_being_dragged != NULL) {
	if (sw->star != NULL) {
	    /* Dragging something into this system, to become a new ship. */
	    sw->star->mouseup();
	} else {
	    /* Dragging a stashitem into this empty system at the
	     * very beginning of the game; it becomes the star. */
	    assert(just_started_new_game);
	    if (type_of_thing_being_dragged == DT_STASHITEM) {
		StashItem *si = (StashItem *)thing_being_dragged;
		sw->add_star(si->piece_size, si->piece_color);
		sw->Layout();
	    }
	    thing_being_dragged = NULL;
	}
    }
}

static void event_within_stash(wxMouseEvent &e, wxPoint &clickpos, StashWidget *sw)
{
    if (e.LeftUp() && thing_being_dragged != NULL) {
	sw->mouseup();
    } else if (e.LeftDown()) {
	assert(thing_being_dragged == NULL);
	wxSizer *gs = sw->GetSizer();
	assert(gs != NULL);
	for (int i=0; i < 12; ++i) {
	    StashItem *si = (StashItem *)gs->GetItem(i)->GetWindow();
	    assert(si != NULL);
	    if (!si->GetScreenRect().Contains(clickpos))
		continue;
	    if (e.LeftDown()) {
		return si->mousedown();
	    }
	}
    }
    assert(thing_being_dragged == NULL);
}

static void event_in_galactic_space(wxMouseEvent &e, GalaxyWidget *gp, const wxPoint &pos)
{
    if (e.LeftUp() && thing_being_dragged != NULL) {
	if (type_of_thing_being_dragged == DT_PIECE) {
	    PieceWidget *pw = (PieceWidget *)thing_being_dragged;
	    if (pw->whose == -1) {
		puts("move a system from point A to point B");
		/* TODO FIXME BUG HACK */
	    } else {
		/* You can't drag a ship into the void, because that would
		 * mean creating a star system with no piece for the star. */
	    }
	} else {
	    assert(type_of_thing_being_dragged == DT_STASHITEM);
	    StashItem *si = (StashItem *)thing_being_dragged;
	    /* Create a new system! */
	    std::string name = gp->get_new_system_name();
	    SystemWidget *np = gp->add_system(pos, name);
	    np->add_star(si->piece_size, si->piece_color);
	    gp->Layout();
	}
	thing_being_dragged = NULL;
    }
}


void GameApp::OnMouseEvent(wxMouseEvent &e)
{
    if (e.LeftDown()) {
	/* wxWidgets says: "The handler of this event should normally
	 * call event.Skip() to allow the default processing to take place
	 * as otherwise the window under mouse wouldn't get the focus." */
	e.Skip();
    }

    wxPoint clickpos = e.GetPosition();
    wxObject *o = e.GetEventObject();
    assert(o != NULL);
    wxPoint frame = ((wxWindow *)o)->GetScreenPosition();
    clickpos += frame;

    /* Figure out what child control the user clicked on. */
    wxWindow *w = wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(w != NULL);
    if (w->GetScreenRect().Contains(clickpos)) {
	GalaxyWidget *gp = (GalaxyWidget *)w;
	for (int i=0; i < gp->num_systems; ++i) {
	    SystemWidget *sw = gp->nth_system(i);
	    if (sw->GetScreenRect().Contains(clickpos))
		return event_within_system(e, clickpos, sw);
	}
	/* Mouseup was somewhere in empty galactic space. */
	return event_in_galactic_space(e, gp, clickpos);
    }
    w = wxWindow::FindWindowById(wxID_ATTACKER_HOMEWORLD_SYSTEM);
    assert(w != NULL);
    if (w->GetScreenRect().Contains(clickpos)) {
	return event_within_system(e, clickpos, (SystemWidget *)w);
    }
    w = wxWindow::FindWindowById(wxID_DEFENDER_HOMEWORLD_SYSTEM);
    assert(w != NULL);
    if (w->GetScreenRect().Contains(clickpos)) {
	return event_within_system(e, clickpos, (SystemWidget *)w);
    }
    w = wxWindow::FindWindowById(wxID_STASH_AREA);
    assert(w != NULL);
    if (w->GetScreenRect().Contains(clickpos)) {
	return event_within_stash(e, clickpos, (StashWidget *)w);
    }

    /* Otherwise the event happened somewhere unimportant. */
    if (e.LeftUp() && thing_being_dragged != NULL) {
	if (type_of_thing_being_dragged == DT_STASHITEM) {
	    /* Cancel the dragging of a StashItem by pretending it was simply
	     * dragged back into the stash. */
	    puts("mouseup nowhere important; canceling dragging of StashItem");
	    StashWidget *sw = (StashWidget *)wxWindow::FindWindowById(wxID_STASH_AREA);
	    assert(sw != NULL);
	    return sw->mouseup();
	}
	thing_being_dragged = NULL;
    }
}
