
#include "wxstuff.h"

PieceWidget::PieceWidget(SystemWidget *p, int id, int who, Size s, Color c) :
    wxPanel(p, id, wxDefaultPosition, wxSize(-1, -1)),
    whose(who),
    piece_size(s), piece_color(c),
    second_size(UNKNOWN_SIZE), second_color(UNKNOWN_COLOR)
{
    this->SetBackgroundStyle(wxBG_STYLE_COLOUR);
    this->SetBackgroundColour(p->GetBackgroundColour());
    this->Connect(wxEVT_PAINT, wxPaintEventHandler(PieceWidget::OnPaint));
    this->Connect(wxEVT_SIZE, wxSizeEventHandler(PieceWidget::OnSize));
}

PieceWidget::PieceWidget(SystemWidget *p, int id, Size s, Color c, Size s2, Color c2) :
    wxPanel(p, id, wxDefaultPosition, wxSize(-1, -1)),
    whose(-1),
    piece_size(s), piece_color(c),
    second_size(s2), second_color(c2)
{
    /* The only PieceWidgets with a second color/size are homeworld stars. */
    this->SetBackgroundStyle(wxBG_STYLE_COLOUR);
    this->SetBackgroundColour(p->GetBackgroundColour());
    this->Connect(wxEVT_PAINT, wxPaintEventHandler(PieceWidget::OnPaint));
    this->Connect(wxEVT_SIZE, wxSizeEventHandler(PieceWidget::OnSize));
}

void setcolor(wxPaintDC &dc, Color c)
{
    wxColour drawcolor;
    switch (c) {
	case RED: drawcolor = wxColour(255,100,100); break;
	case YELLOW: drawcolor = wxColour(255,255,0); break;
	case GREEN: drawcolor = wxColour(50,255,50); break;
	case BLUE: drawcolor = wxColour(100,100,255); break;
	default: assert(false);
    }
    dc.SetPen(wxPen(drawcolor));
    dc.SetBrush(wxBrush(drawcolor));
}

static int getoffset(int w, Size s)
{
    switch (s) {
	case SMALL: return 3*w/8;
	case MEDIUM: return 2*w/8;
	case LARGE: return w/8;
	default: assert(false);
    }
}

static void drawsquare(wxPaintDC &dc, int w, int h, Size s, Color c, bool justbox)
{
    setcolor(dc, c);
    if (justbox)
      dc.SetPen(wxPen(wxColour(0,0,0)));
    wxPoint points[4];
    int offset = getoffset(w, s);
    points[0] = wxPoint(offset, offset);
    points[1] = wxPoint(offset, h-offset);
    points[2] = wxPoint(w-offset, h-offset);
    points[3] = wxPoint(w-offset, offset);
    dc.DrawPolygon(4, points);
}

void PieceWidget::OnPaint(wxPaintEvent &unused)
{
    wxPaintDC dc(this);
    wxSize size = this->GetSize();
    int w = size.GetWidth();
    int h = size.GetHeight();
    wxColour drawcolor;

    if (this->whose == -1) {
	/* it's a star */
	const bool samecolor = (this->piece_color == this->second_color);
	if (this->second_size == UNKNOWN_SIZE) {
	    drawsquare(dc, w, h, this->piece_size, this->piece_color, false);
	} else if (this->second_size > this->piece_size) {
	    drawsquare(dc, w, h, this->second_size, this->second_color, false);
	    drawsquare(dc, w, h, this->piece_size, this->piece_color, samecolor);
	} else if (this->second_size < this->piece_size) {
	    drawsquare(dc, w, h, this->piece_size, this->piece_color, false);
	    drawsquare(dc, w, h, this->second_size, this->second_color, samecolor);
	} else {
	    assert(this->second_size == this->piece_size);
	    /* In this case we draw a single square divided diagonally. */
	    drawsquare(dc, w, h, this->piece_size, this->piece_color, false);
	    wxPoint points[3];
	    int offset = getoffset(w, this->piece_size);
	    points[0] = wxPoint(offset, offset);
	    points[1] = wxPoint(w-offset, h-offset);
	    points[2] = wxPoint(offset, h-offset);
	    if (samecolor) {
		dc.SetPen(wxPen(wxColour(0,0,0)));
		dc.DrawLines(2, points);
	    } else {
		setcolor(dc, this->second_color);
		dc.DrawPolygon(3, points);
	    }
	}
	return;
    } else {
	assert(this->second_size == UNKNOWN_SIZE);
	assert(this->second_color == UNKNOWN_COLOR);
    }

    wxPoint points[3];
    if (whose == 1) {
	/* it points south */
	switch (this->piece_size) {
	    case SMALL:
		points[0] = wxPoint(w/2, h/2);
		points[1] = wxPoint(19*w/32, h/8);
		points[2] = wxPoint(13*w/32, h/8);
		break;
	    case MEDIUM:
		points[0] = wxPoint(w/2, 3*h/4);
		points[1] = wxPoint(11*w/32, h/8);
		points[2] = wxPoint(21*w/32, h/8);
		break;
	    case LARGE:
		points[0] = wxPoint(w/2, h);
		points[1] = wxPoint(3*w/4, 0);
		points[2] = wxPoint(w/4, 0);
		break;
	    default: assert(false);
	}
    } else {
	assert(whose == 0);
	/* it points north */
	switch (this->piece_size) {
	    case SMALL:
		points[0] = wxPoint(w/2, h/2);
		points[1] = wxPoint(19*w/32, 7*h/8);
		points[2] = wxPoint(13*w/32, 7*h/8);
		break;
	    case MEDIUM:
		points[0] = wxPoint(w/2, h/4);
		points[1] = wxPoint(11*w/32, 7*h/8);
		points[2] = wxPoint(21*w/32, 7*h/8);
		break;
	    case LARGE:
		points[0] = wxPoint(w/2, 0);
		points[1] = wxPoint(3*w/4, h);
		points[2] = wxPoint(w/4, h);
		break;
	    default: assert(false);
	}
    }
    setcolor(dc, this->piece_color);
    dc.DrawPolygon(3, points);
}

void PieceWidget::OnSize(wxSizeEvent &unused)
{
    this->Refresh();
}

void PieceWidget::mouseup()
{
    assert(thing_being_dragged != NULL);
    SystemWidget *sw = (SystemWidget *)this->GetParent();
    assert(sw != NULL);
    if (type_of_thing_being_dragged == DT_STASHITEM) {
	StashItem *si = (StashItem *)thing_being_dragged;
	StashWidget *stash = (StashWidget *)si->GetParent();
	GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
	if (this->whose == global_attacker &&
		si->piece_size == this->piece_size &&
		si->piece_color != this->piece_color) {
	    /* Dragging a piece from the stash onto a ship of the same size but
	     * different color causes the two pieces to swap places. */
	    stash->add_piece(this->piece_size, this->piece_color);
	    this->piece_color = si->piece_color;
	    stash->Layout();
	} else if (just_started_new_game && this->whose == -1 &&
		this->second_color == UNKNOWN_COLOR &&
		(sw == gp->attacker_homeworld || sw == gp->defender_homeworld)) {
	    /* Dragging a piece from the stash onto a single-piece star at
	     * the very beginning of the game stacks the pieces into a binary
	     * star. This is the only time such an action is allowed. */
	    assert(this == sw->star);
	    this->second_color = si->piece_color;
	    this->second_size = si->piece_size;
	} else {
	    /* Dragging a piece from the stash onto anywhere else in a star system
	     * causes it to become a new ship. */
	    sw->add_ship(global_attacker, si->piece_size, si->piece_color);
	}
	sw->Layout();
    } else {
	assert(type_of_thing_being_dragged == DT_PIECE);
	PieceWidget *pw = (PieceWidget *)thing_being_dragged;
	SystemWidget *old_sw = (SystemWidget *)pw->GetParent();
	if (pw->whose == global_attacker) {
	    /* Dragging a friendly ship from one system to another causes it
	     * to disappear from the old system and reappear here. */
	    if (old_sw != sw) {
		old_sw->GetSizer()->Detach(pw);
		pw->Reparent(sw);
		sw->add_ship(pw);
		old_sw->Layout();
		sw->Layout();
	    }
	}
	/* Dragging an unfriendly ship to another system is not allowed.
	 * Dragging a star to another system is not allowed. */
    }
    thing_being_dragged = NULL;
}
