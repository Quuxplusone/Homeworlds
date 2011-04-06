
#include "wxstuff.h"

SystemWidget::SystemWidget(wxWindow *p, int id, wxPoint pos) :
    wxPanel(p, id, pos, wxDefaultSize, wxBORDER_SUNKEN)
{
    this->SetBackgroundStyle(wxBG_STYLE_COLOUR);
    this->SetBackgroundColour(wxColour(32,32,32));
    this->SetMinSize(wxSize(50,50));
    wxBoxSizer *hbox = new wxBoxSizer(wxHORIZONTAL);
    this->SetSizer(hbox);
    this->star = NULL;
}

void SystemWidget::add_star(PieceWidget *pw)
{
    assert(this->star == NULL);
    this->star = pw;
    this->GetSizer()->Add(this->star, /*proportion=*/1,
	    wxSHAPED | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT,
	    /*border=*/0);
}

void SystemWidget::add_star(Size s, Color c)
{
    this->add_star(new PieceWidget(this, wxID_ANY, -1, s, c));
}

void SystemWidget::add_star(Size s1, Color c1, Size s2, Color c2)
{
    this->add_star(new PieceWidget(this, wxID_ANY, s1, c1, s2, c2));
}

void SystemWidget::add_ship(PieceWidget *pw)
{
    this->GetSizer()->Add(pw, /*proportion=*/1,
	    wxSHAPED | wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT,
	    /*border=*/0);
}

void SystemWidget::add_ship(int who, Size s, Color c)
{
    this->add_ship(new PieceWidget(this, wxID_ANY, who, s, c));
}

void SystemWidget::update(const StarSystem *sys)
{
    wxSizer *hbox = this->GetSizer();
    this->star = NULL;
    hbox->Clear(true);

    if (sys == NULL)
	return;

    this->name = sys->name;
    Color c1 = UNKNOWN_COLOR;
    Size s1 = UNKNOWN_SIZE;
    Color c2 = UNKNOWN_COLOR;
    Size s2 = UNKNOWN_SIZE;
    for (Color c = RED; c <= BLUE; ++c) {
	for (Size s = SMALL; s <= LARGE; ++s) {
	    int n = sys->star.numberOf(c,s);
	    if (n == 2) {
		s1 = s; c1 = c;
		s2 = s; c2 = c;
		goto done;
	    } else if (n == 1) {
		if (s1 == UNKNOWN_SIZE) {
		    s1 = s; c1 = c;
		    continue;
		} else {
		    s2 = s; c2 = c;
		    goto done;
		}
	    } else {
		assert(n == 0);
	    }
	}
    }
  done:
    if (s2 == UNKNOWN_SIZE) {
	this->add_star(s1, c1);
    } else {
	this->add_star(s1, c1, s2, c2);
    }

    /* Now add the ships. */
    for (int who = 0; who <= 1; ++who) {
	const PieceCollection &pc = sys->ships[who];
	for (Color c = RED; c <= BLUE; ++c) {
	    for (Size s = SMALL; s <= LARGE; ++s) {
		int n = pc.numberOf(c,s);
		for (int i=0; i < n; ++i)
		    this->add_ship(who, s, c);
	    }
	}
    }
}

