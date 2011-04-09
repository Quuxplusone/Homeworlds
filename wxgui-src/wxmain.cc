
#include <assert.h>
#include <stdarg.h>
#include <string>
#include <vector>
#include "state.h"
#include "move.h"
#include "AllMoves.h"
#include "ApplyMove.h"
#include "InferMove.h"
#include "PlanetNames.h"
#include "getline.h"
#include "AI.h"

#include <wx/wx.h>
#include <wx/menu.h>
#include <wx/panel.h>
#include <wx/ffile.h>
#include "wxstuff.h"

#define randint0(n) (rand() % (n))


class History {
    struct Node {
        WholeMove move;
        GameState st;     /* the state *after* this move */
    };
    /* hvec[0] is the initial state; its "move" is garbage; it can't be undone. */
    std::vector<Node> hvec;
    /* hvec[hidx] is the last (non-undone) move. */
    int hidx;
  public:
    History(): hidx(0) {
        hvec.push_back(Node());
    }
    void setup(const GameState &st) {
	hidx = 0;
	hvec.clear();
	hvec.push_back(Node());
        hvec[0].st = st;
    }
    void review(FILE *fp) {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        fprintf(fp, "%s", hvec[0].st.toString().c_str());
        for (int i=1; i <= hidx; ++i) {
            fprintf(fp, "%s\n", hvec[i].move.toString().c_str());
        }
    }
    bool undo() {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (hidx == 0) {
            return false;
        } else {
            --hidx;
            return true;
        }
    }
    bool redo() {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        if (hidx+1 == (int)hvec.size()) {
            return false;
        } else {
            ++hidx;
            return true;
        }
    }
    int current_attacker() {
        assert(hidx >= 0);
        assert(hidx < (int)hvec.size());
        return (hidx % 2);
    }
    void makemove(const WholeMove &move, int attacker) {
        assert(attacker == current_attacker());
        hvec.resize(hidx+1);
        hvec.push_back(hvec[hidx]);
        ++hidx;
        const bool UNUSED(success) = ApplyMove::Whole(hvec[hidx].st, attacker, move);
        assert(success);
        hvec[hidx].move = move;
    }
    const GameState &currentstate() const {
        return hvec[hidx].st;
    }
};

struct GameApp : public wxApp
{
    wxFrame *mainwindow;
    wxPanel *leftpanel;

    History history;
    /* The "shadow" game state for displaying to the user. */
    std::vector<SystemWidget *> stars;

    void new_game();
    void ai_starting_position();

    /* Magical entry point. */
    bool OnInit();

    /* Event handlers. */
    void OnMouseEvent(wxMouseEvent &e);
    void OnNewGame(wxCommandEvent &) { new_game(); }
    void load_game(wxCommandEvent &);
    void undo_move(wxCommandEvent &);
    void redo_move(wxCommandEvent &);
    void done_starting_position();
    void done_move(wxCommandEvent &);
    void clear_move(wxCommandEvent &);
    void ai_move(wxCommandEvent &);
    void clicked_quit(wxCommandEvent &) { mainwindow->Close(true); }
    void about(wxCommandEvent &);
};


/* wxWidgets calls this method on startup. */
bool GameApp::OnInit()
{
    mainwindow = new wxFrame(NULL, wxID_MAINWINDOW, wxT("Homeworlds"), wxDefaultPosition, wxSize(700, 400));

    wxMenuBar *menubar = new wxMenuBar;
    wxMenu *filemenu = new wxMenu;
    wxMenu *editmenu = new wxMenu;
    wxMenu *helpmenu = new wxMenu;
    menubar->Append(filemenu, wxT("&File"));
      filemenu->Append(wxID_NEW, wxT("&New Game"), wxT("Starts a new game"));
      filemenu->Append(wxID_OPEN, wxT("&Open Game"), wxT("Opens a saved game"));
      filemenu->Append(wxID_SAVEAS, wxT("&Save Game"), wxT("Saves a transcript of the current game to a text file"));
      filemenu->AppendSeparator();
      filemenu->Append(wxID_EXIT, wxT("&Quit"), wxT("Quits the program"));
    menubar->Append(editmenu, wxT("&Edit"));
      editmenu->Append(wxID_DONE_MOVE, wxT("&Done\tENTER"), wxT("Signals that you're ready to submit the current move"));
      editmenu->Append(wxID_CLEAR_MOVE, wxT("&Reset\tR"), wxT("Reset the board position to the way it was before you started this move"));
      filemenu->AppendSeparator();
      editmenu->Append(wxID_AI_MOVE, wxT("&AI Move\tA"), wxT("Let the AI player make a move"));
      filemenu->AppendSeparator();
      editmenu->Append(wxID_UNDO, wxT("&Undo Last Move"), wxT("Undoes the last whole move"));
      editmenu->Append(wxID_REDO, wxT("&Redo Move"), wxT("Redoes the last whole move"));
    menubar->Append(helpmenu, wxT("&Help"));
      helpmenu->Append(wxID_ABOUT, wxT("&About..."), wxT("Displays a short copyright message"));
      helpmenu->Append(wxID_HELP, wxT("&Help\tF1"), wxT("Help about the game and how to play"));
    mainwindow->SetMenuBar(menubar);
    mainwindow->CreateStatusBar();

    this->Connect(wxID_EXIT, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::clicked_quit));
    this->Connect(wxID_NEW, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::OnNewGame));
    this->Connect(wxID_OPEN, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::load_game));
    this->Connect(wxID_DONE_MOVE, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::done_move));
    this->Connect(wxID_CLEAR_MOVE, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::clear_move));
    this->Connect(wxID_AI_MOVE, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::ai_move));
    this->Connect(wxID_UNDO, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::undo_move));
    this->Connect(wxID_REDO, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::redo_move));
    this->Connect(wxID_ABOUT, wxEVT_COMMAND_MENU_SELECTED, wxCommandEventHandler(GameApp::about));

    this->Connect(wxEVT_LEFT_UP, wxMouseEventHandler(GameApp::OnMouseEvent));
    this->Connect(wxEVT_LEFT_DOWN, wxMouseEventHandler(GameApp::OnMouseEvent));
    this->Connect(wxEVT_LEFT_DCLICK, wxMouseEventHandler(GameApp::OnMouseEvent));
    
    this->leftpanel = new wxPanel(mainwindow, wxID_ANY, wxDefaultPosition, wxDefaultSize);

    GalaxyWidget *gp = new GalaxyWidget(leftpanel, wxID_GALAXY_MAP);
    gp->attacker_homeworld = new SystemWidget(leftpanel, wxID_ATTACKER_HOMEWORLD_SYSTEM, wxDefaultPosition);
    gp->attacker_homeworld->name = "Fry";
    gp->defender_homeworld = new SystemWidget(leftpanel, wxID_DEFENDER_HOMEWORLD_SYSTEM, wxDefaultPosition);
    gp->defender_homeworld->name = "Bender";
    gp->stash = new StashWidget(mainwindow, wxID_STASH_AREA);

    leftpanel->SetSizer(new wxBoxSizer(wxVERTICAL));
    leftpanel->GetSizer()->Add(gp->defender_homeworld, 1, wxEXPAND | wxALL, /*border=*/5);
    leftpanel->GetSizer()->Add(gp, 4, wxEXPAND | wxALL, /*border=*/5);
    leftpanel->GetSizer()->Add(gp->attacker_homeworld, 1, wxEXPAND | wxALL, /*border=*/5);
    leftpanel->GetSizer()->Fit(leftpanel);
    
    mainwindow->SetSizer(new wxBoxSizer(wxHORIZONTAL));
    mainwindow->GetSizer()->Add(leftpanel, 3, wxEXPAND, /*border=*/0);
    mainwindow->GetSizer()->Add(gp->stash, 1, wxEXPAND | wxALL, /*border=*/5);
    mainwindow->GetSizer()->Fit(mainwindow);

    mainwindow->Show(true);

    new_game();

    return true;
}


/* Notice that this version of do_error() does NOT abort the program! */
static void do_error(const std::string &msg)
{
    wxString wmsg(msg.c_str(), wxConvLocal);
    wxMessageDialog mdg(NULL, wmsg, wxT("Error"), wxOK | wxICON_ERROR);
    mdg.ShowModal();
}


void GameApp::new_game()
{
    GameState ng;
    ng.newGame();
    this->history.setup(ng);
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    assert(gp->attacker_homeworld != NULL);
    assert(gp->stash != NULL);
    gp->update(this->history.currentstate());
    global_attacker = this->history.current_attacker();
    mainwindow->SetTitle(wxT("Homeworlds - Untitled Game"));
    mainwindow->SetStatusText(wxT("Player 1 set up."));
    mainwindow->Layout();
    just_started_new_game = true;
}


void GameApp::load_game(wxCommandEvent &)
{
    wxFileDialog fdg(this->mainwindow);
    fdg.SetWindowStyleFlag(wxFD_OPEN | wxFD_FILE_MUST_EXIST);
    if (fdg.ShowModal() == wxID_CANCEL)
        return;
    wxString fullname = fdg.GetPath();
    wxString basename = fdg.GetFilename();
    wxFFile in(fullname, wxT("r"));
    if (!in.IsOpened()) {
        wxMessageDialog mdg(NULL, wxT("File not found"), wxT("Error"), wxOK | wxICON_ERROR);
        mdg.ShowModal();
	return;
    }
    GameState initialState;
    std::string firstline = initialState.scan(in.fp());
    firstline += "\n";
    for (int i = firstline.length(); i > 0; --i) {
	int rc = ungetc(firstline[i-1], in.fp());
	if (rc == EOF) {
	    do_error(mprintf("ungetc: %d character%s of input could not be pushed back.\n"
	            "Try adding a blank line after the homeworld setup lines.", i,
	            (i>1 ? "s" : "")));
	    return;
	}
    }
    assignPlanetNames(initialState, NULL);
    StarSystem *hw = initialState.homeworldOf(0);
    if (hw == NULL) {
	do_error("The initial homeworld setup didn't include Player 0's homeworld!");
	return;
    }
    hw = initialState.homeworldOf(1);
    if (hw == NULL) {
	do_error("The initial homeworld setup didn't include Player 1's homeworld!");
	return;
    }

    puts("aaa");
    
    History newhistory;
    newhistory.setup(initialState);
    /* Now read the history of the new game, move by move. */
    int attacker = 0;
    char *moveline = NULL;
    while (1) {
	char *result = fgetline_113(&moveline, in.fp());
	if (result == NULL)
	    break;
	assert(result == moveline);
	if (moveline[0] == '#' || moveline[0] == '\0') {
	    /* This is a comment; ignore it. */
	    free(moveline);
	    continue;
	} else if (newhistory.currentstate().gameIsOver()) {
	    do_error("The transcript file contains additional moves after the end of the game!");
	    free(moveline);
	    return;
	}
	WholeMove move;
	if (!move.scan(moveline)) {
	    do_error(mprintf("The transcript file contains errors. The following line didn't parse as a move:\n%s", moveline));
	    free(moveline);
	    return;
	}
	if (move.is_missing_pieces()) {
	    WholeMove oldmove = move;
	    const bool inferred = inferMoveFromState(newhistory.currentstate(), attacker, move);
	    if (!inferred) {
		/* We couldn't infer the user's intended move. Just restore the old move,
		 * with the un-filled-in blanks, and let isValidMove() reject it below. */
		move = oldmove;
	    }
	}
	free(moveline);
	/* If we've gotten this far, the file gave us a syntactically
	 * correct move. Try to apply it; if it's semantically invalid or
	 * illegal, reject it. */
	const bool success = ApplyMove::isValidMove(newhistory.currentstate(), attacker, move);
	if (!success) {
	    do_error(mprintf("The transcript file contains errors. The following move is not legal:\n%s", move.toString().c_str()));
	    return;
	}
	/* We got a completely valid move. */
	newhistory.makemove(move, attacker);
	attacker = (1 - attacker);
    }

    /* We've read the entire file, and apparently it was all valid. That
     * means success! Replace the current game with the newly loaded game. */
    this->history = newhistory;

    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    gp->update(this->history.currentstate());
    global_attacker = this->history.current_attacker();
    just_started_new_game = false;
    mainwindow->SetTitle(wxT("Homeworlds - ") + basename);
    mainwindow->SetStatusText(wxT(""));
    mainwindow->Layout();
}

void GameApp::done_starting_position()
{
    assert(just_started_new_game);
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    SystemWidget *my_homeworld = (global_attacker == 0) ? gp->attacker_homeworld : gp->defender_homeworld;
    assert(my_homeworld != NULL);
    if (my_homeworld->star == NULL)
      return do_error("You haven't selected a star for your homeworld system!");
    wxSizer *gs = my_homeworld->GetSizer();
    wxSizerItem *it = gs->GetItem(1);
    if (it == NULL)
      return do_error("You haven't selected a ship for your homeworld system!");
    assert(it->GetWindow() != NULL);
    assert(((PieceWidget*)it->GetWindow())->whose == global_attacker);
    if (gs->GetItem(2)) {
	/* Allow the user to set up more interesting positions, but warn
	 * about them. */
	wxMessageDialog mdg(NULL,
		wxT("Your homeworld system should contain only one starting ship."
			" Do you want to continue with this easier setup anyway?"),
		wxT("Error"), wxYES_NO | wxNO_DEFAULT | wxICON_ERROR);
	if (mdg.ShowModal() != wxID_YES)
	    return;
    }
    if (gp->num_systems != 0) {
	/* Allow the user to set up more interesting positions, but warn
	 * about them. */
	wxMessageDialog mdg(NULL,
		wxT("The starting position should not contain any star systems"
			" besides the two homeworlds."
			" Do you want to continue with this setup anyway?"),
		wxT("Error"), wxYES_NO | wxNO_DEFAULT | wxICON_ERROR);
	if (mdg.ShowModal() != wxID_YES)
	    return;
    }
    
    if (global_attacker == 0) {
	global_attacker = 1;
	mainwindow->SetStatusText(wxT("Player 2 set up."));
    } else {
	GameState st = gp->to_state();
	this->history.setup(st);
	global_attacker = 0;
	just_started_new_game = false;
	mainwindow->SetStatusText(wxT(""));
    }
    mainwindow->Layout();
}

/* Helper function for done_move().  Assign names to the new star systems
 * created by m, in order to satisfy "st + m = newst". */
static bool reassign_a_name_cleverly(const GameState &st, WholeMove &m,
	const GameState &target)
{
    int n = target.stars.size();
    GameState stplusm = st;
    ApplyMove::or_die(stplusm, global_attacker, m);
    assert(n == (int)stplusm.stars.size());
    /* For each star system in stplusm that's not in target... it must have
     * come from a MOVE_CREATE action in m. We're going to need to rename
     * that system according to whatever its name is in "target". */
    for (int i=0; i < n; ++i) {
	const StarSystem &ss = stplusm.stars[i];
	if (target.systemNamed(ss.name.c_str()) != NULL)
	  continue;
	/* Find a system in target that looks just like this one,
	 * and which doesn't appear by name in st. */
	for (int j=0; j < n; ++j) {
	    const StarSystem &targetss = target.stars[j];
	    if (stplusm.systemNamed(targetss.name.c_str()))
	      continue;
	    if (targetss.toComparableString() != ss.toComparableString())
	      continue;
	    /* Okay, we can map ss.name to targetss.name. */
	    printf("mapping %s to %s\n", ss.name.c_str(), targetss.name.c_str());
	    for (int k=0; k < (int)m.actions.size(); ++k) {
		if (m.actions[k].where == ss.name)
		  m.actions[k].where = targetss.name;
		if (m.actions[k].whither == ss.name)
		  m.actions[k].whither = targetss.name;
	    }
	    return true;
	}
	/* If we got here, it means that no system identical to ss appeared
	 * in target, even though ss appeared in stplusm (and stplusm was
	 * supposed to be identical to target).  That's impossible. */
	assert(false);
    }
    /* If we got all the way through that loop, then stplusm is exactly
     * identical to target, even considering system names --- but it could
     * legitimately still have a different toString(), depending on the
     * ordering of systems within the "stars" vector. */
    return false;
}


void GameApp::done_move(wxCommandEvent &)
{
    if (just_started_new_game) {
	done_starting_position();
	return;
    }
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    GameState oldstate = this->history.currentstate();
    printf("old state is:\n%s\n", oldstate.toString().c_str());
    GameState targetstate = gp->to_state();
    printf("target state is:\n%s\n", targetstate.toString().c_str());
    std::string target = targetstate.toComparableString();
    std::vector<WholeMove> allmoves;
    findAllMoves(oldstate, global_attacker, allmoves,
	    /*prune_obviously_worse_moves=*/false,
	    /*look_only_for_wins=*/false,
	    /*these_colors_only=*/0xF);
    WholeMove successful_move;
    for (int i=0; i < (int)allmoves.size(); ++i) {
	GameState newst = oldstate;
	WholeMove &m = allmoves[i];
	ApplyMove::or_die(newst, global_attacker, m);
	if (newst.toComparableString() == target) {
	    successful_move = m;
	    goto success;
	}
    }
    /* Failure. The new state isn't legally reachable from the old state. */
    return do_error("The current board position isn't legally reachable from"
	    " the state at the end of your opponent's last turn. Check for"
	    " unoccupied star systems, or use \"Edit -> Reset\" to restart"
	    " your current move from the beginning.");
  success:
    assert(ApplyMove::isValidMove(this->history.currentstate(), global_attacker, successful_move));
    /* Consider this!
     *   successful_move has star names like "Uuaaaaaa".
     *   The actual SystemWidgets have names like "Alpha".
     *   Simply calling reassign_planet_names() on this move will give it
     *     names like "Delos".
     * We really need some way to reassign names cleverly so that
     * successful_move will wind up with the exact same names that each
     * SystemWidget wound up with. */
    for (int i=0; i < 4; ++i) {
	if (!reassign_a_name_cleverly(oldstate, successful_move, targetstate))
	    break;
	/* After reassigning three names, we should really be finished.
	 * If we get here a fourth time, assert failure; something is wrong. */
	assert(i != 3);
    }
    this->history.makemove(successful_move, global_attacker);
    global_attacker = (1 - global_attacker);
    wxString wmsg(successful_move.toString().c_str(), wxConvLocal);
    mainwindow->SetStatusText(wmsg);
    mainwindow->Layout();
}

void GameApp::clear_move(wxCommandEvent &)
{
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    gp->update(this->history.currentstate());
    mainwindow->SetStatusText(wxT(""));
}


static bool setup_ai(GameState &st, StarSystem &hw)
{
    assert(&hw == &st.stars.back());
    const int attacker = hw.homeworldOf;
    assert(attacker == 0 || attacker == 1);
    const StarSystem * const opponent_hw = st.homeworldOf(1-attacker);

    /* Try 20 times, then bail out. */
    for (int i=0; i < 20; ++i) {
        hw.star.clear();
        hw.ships[attacker].clear();
        /* The star should contain two colors, including at least one yellow or blue. */
        const Color c1 = (randint0(2)==0 ? YELLOW : BLUE);
        const Color c2 = Color((1+randint0(3)+(int)c1) % 4);
        /* The star should contain two different sizes. */
        const Size s1 = Size(randint0(3));
        const Size s2 = Size((s1+1+randint0(2)) % 3);
        hw.star.insert(c1, s1);
        hw.star.insert(c2, s2);
        if (opponent_hw != NULL) {
            /* Make sure the homeworlds are as far apart as possible.
             * This condition disallows (r1y1, g2b3) and (r1y2, g1b2)
             * while still allowing (r1y1, g1b2) and (r1y2, g1b3). */
            if (opponent_hw->star.numberOf(s1) == opponent_hw->star.numberOf(s2))
              continue;
        }
        /* The initial ship should be big and non-red. If the star doesn't have green
         * already, then the initial ship must be green. Otherwise, the ship must be
         * of a color that's not already in the star itself. */
        const Color shipc = (c2 != GREEN) ? GREEN : (c1 == YELLOW) ? BLUE : YELLOW;
        hw.ships[attacker].insert(shipc, LARGE);
        /* If this configuration isn't actually possible, rinse and repeat. */
        if (!st.stash.contains(hw.pieceCollection()))
          continue;
        /* This configuration is okay. */
	st.stash -= hw.star;
	st.stash -= hw.ships[attacker];
        return true;
    }
    /* We couldn't find a reasonable set of pieces that was still left in the
     * stash. This can only be due to malicious fiddling by the user, but
     * we'll give him a nice error message anyway. */
    return false;
}

void GameApp::ai_starting_position()
{
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    assert(just_started_new_game);
    if (global_attacker == 0) {
	GameState st;
	st.newGame();
	st.stars.push_back(StarSystem("Fry"));
	StarSystem &hw = st.stars.back();
	hw.homeworldOf = 0;
	const bool UNUSED(success) = setup_ai(st, hw);
	assert(success);
	gp->update(st);
	global_attacker = 1;
	assert(just_started_new_game);
    } else {
	assert(global_attacker == 1);
	GameState st = gp->to_state();
	if (st.homeworldOf(0) == NULL || st.homeworldOf(0)->ships[0].empty()) {
	    do_error("The AI cannot create a homeworld system for Player 1 when Player 0's homeworld system is empty!");
	    return;
	}
	assert(st.homeworldOf(0) != NULL);
	StarSystem *hw = st.homeworldOf(1);
	if (hw == NULL) {
	    st.stars.push_back(StarSystem("Bender"));
	    hw = &st.stars.back();
	    hw->homeworldOf = 1;
	}
	if (setup_ai(st, *hw)) {
	    this->history.setup(st);
	    assert(this->history.current_attacker() == 0);
	    global_attacker = 0;
	    gp->update(this->history.currentstate());
	    just_started_new_game = false;
	} else {
	    do_error("The AI cannot create a homeworld system for Player 1"
		    " due to all the additional pieces on the map. Put some"
		    " pieces back in the stash and try again.");
	}
    }
}

void GameApp::ai_move(wxCommandEvent &)
{
    if (just_started_new_game) {
	/* We're being asked to set up a homeworld system. */
	GameApp::ai_starting_position();
	return;
    }
    GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
    assert(gp != NULL);
    GameState st = this->history.currentstate();
    if (st.gameIsOver()) {
	mainwindow->SetStatusText(wxT("The game is already over!"));
	return;
    }
    WholeMove bestmove = get_ai_move(st, global_attacker);
    ApplyMove::or_die(st, global_attacker, bestmove);
    this->history.makemove(bestmove, global_attacker);
    global_attacker = this->history.current_attacker();
    gp->update(this->history.currentstate());
    wxString wmsg(bestmove.toString().c_str(), wxConvLocal);
    mainwindow->SetStatusText(wmsg);
}

void GameApp::undo_move(wxCommandEvent &)
{
    const bool success = this->history.undo();
    if (success) {
	GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
	assert(gp != NULL);
	gp->update(this->history.currentstate());
	global_attacker = this->history.current_attacker();
	mainwindow->SetStatusText(wxT(""));
    } else {
	mainwindow->SetStatusText(wxT("There are no moves to be undone!"));
    }
}


void GameApp::redo_move(wxCommandEvent &)
{
    const bool success = this->history.redo();
    if (success) {
	GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
	assert(gp != NULL);
	gp->update(this->history.currentstate());
	global_attacker = this->history.current_attacker();
	mainwindow->SetStatusText(wxT(""));
    } else {
	mainwindow->SetStatusText(wxT("There are no moves to be redone!"));
    }
}


void GameApp::about(wxCommandEvent &)
{
    mainwindow->SetStatusText(wxT(""));
    wxMessageDialog adg(NULL,
        wxT("Homeworlds GUI AI\nArthur O'Dwyer\nFreeware\n\nHomeworlds is a chess-like game of strategy and tactics. For more information, see the Help menu."),
        wxT("About Homeworlds"),
        wxOK | wxICON_INFORMATION);
    adg.ShowModal();
}

/* This is a magic incantation, i.e., boilerplate. It instructs wxWidgets to
 * call GameApp::OnInit() as soon as the widgets library is up and running. */
IMPLEMENT_APP(GameApp)
