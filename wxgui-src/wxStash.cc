
#include "wxstuff.h"
#include <wx/sizer.h>

StashItem::StashItem(StashWidget *p, Color c, Size s, int n_) :
    wxPanel(p, wxID_ANY, wxDefaultPosition, wxSize(50,75)),
    piece_color(c), piece_size(s), n(n_)
{
    this->SetBackgroundStyle(wxBG_STYLE_COLOUR);
    this->SetBackgroundColour(p->GetBackgroundColour());
    this->Connect(wxEVT_PAINT, wxPaintEventHandler(StashItem::OnPaint));
    this->Connect(wxEVT_SIZE, wxSizeEventHandler(StashItem::OnSize));
}


StashWidget::StashWidget(wxWindow *p, int id) :
    wxWindow(p, id, wxDefaultPosition, wxSize(200,320), wxBORDER_SUNKEN)
{
    this->SetBackgroundStyle(wxBG_STYLE_COLOUR);
    this->SetBackgroundColour(wxColour(0,0,0));
    wxGridSizer *gs = new wxGridSizer(/*rows=*/4, /*cols=*/3, /*vgap=*/2, /*hgap=*/0);
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            gs->Add(new StashItem(this, c, s, 0),
                    /*proportion=*/1,
                    wxSHAPED | wxALIGN_CENTER);
        }
    }
    this->SetSizerAndFit(gs);
}

void StashWidget::update(const PieceCollection &pc)
{
    wxSizer *gs = this->GetSizer();
    assert(gs != NULL);
    int i = 0;
    for (Color c = RED; c <= BLUE; ++c) {
        for (Size s = SMALL; s <= LARGE; ++s) {
            StashItem *si = (StashItem *)gs->GetItem(i++)->GetWindow();
            assert(si != NULL);
            si->n = pc.numberOf(c, s);
        }
    }
}

void StashWidget::add_piece(Size s, Color c)
{
    wxSizer *gs = this->GetSizer();
    assert(gs != NULL);
    int idx = 3*(int)c + (int)s;
    wxSizerItem *it = gs->GetItem(idx);
    assert(it != NULL);
    StashItem *si = (StashItem *)it->GetWindow();
    assert(si != NULL);
    assert(0 <= si->n && si->n <= 2);
    si->n += 1;
}

void StashItem::OnPaint(wxPaintEvent &)
{
    wxPaintDC dc(this);
    wxSize size = this->GetSize();
    int w = size.GetWidth();
    int h = size.GetHeight()/2;
    wxColour drawcolor;

    wxPoint points[3];
    switch (this->piece_size) {
        case SMALL:
            points[0] = wxPoint(w/2, h/2);
            points[1] = wxPoint(12*w/32, h);
            points[2] = wxPoint(20*w/32, h);
            break;
        case MEDIUM:
            points[0] = wxPoint(w/2, h/4);
            points[1] = wxPoint(10*w/32, h);
            points[2] = wxPoint(22*w/32, h);
            break;
        case LARGE:
            points[0] = wxPoint(w/2, 0);
            points[1] = wxPoint(8*w/32, h);
            points[2] = wxPoint(24*w/32, h);
            break;
        default: assert(false);
    }
    points[0].y += (size.GetHeight() - h);
    points[1].y += (size.GetHeight() - h);
    points[2].y += (size.GetHeight() - h);
    setcolor(dc, this->piece_color);
    dc.SetPen(wxPen(wxColour(0,0,0)));
    for (int i=0; i < n; ++i) {
        points[0].y -= h/5;
        points[1].y -= h/5;
        points[2].y -= h/5;
        dc.DrawPolygon(3, points);
    }
}

void StashItem::OnSize(wxSizeEvent &)
{
    this->Refresh();
}

void StashItem::mousedown()
{
    assert(thing_being_dragged == NULL);
    if (n > 0) {
        thing_being_dragged = this;
        type_of_thing_being_dragged = DT_STASHITEM;
        n -= 1;
        this->Refresh();
    }
}


void StashWidget::mouseup()
{
    assert(thing_being_dragged != NULL);
    switch (type_of_thing_being_dragged) {
        case DT_PIECE: {
            /* We're dropping a PieceWidget into the stash. */
            PieceWidget *pw = (PieceWidget *)thing_being_dragged;
            SystemWidget *sw = (SystemWidget *)pw->GetParent();
            assert(sw != NULL);
            if (pw->whose == -1) {
                /* We're dropping a whole star system into the stash. */
                GalaxyWidget *gp = (GalaxyWidget *)wxWindow::FindWindowById(wxID_GALAXY_MAP);
                assert(gp != NULL);
                assert(this == gp->stash);
                wxSizer *gs = sw->GetSizer();
                if (gs->GetItem(1) == NULL) {
                    /* The system contains no ships. It's okay to destroy it. */
                    assert(gs->GetItem((size_t)0)->GetWindow() == sw->star);
                    gp->restash_and_delete_system(sw);
                } else {
                    printf("dragged occupied system %s into stash; likely accidental, so do nothing\n", sw->name.c_str());
                }
            } else {
                /* We're dropping just one piece. */
                this->add_piece(pw->piece_size, pw->piece_color);
                this->Refresh();
#ifdef __WXMAC__
                // temporary patch for OSX
                pw->Hide();
#endif
                sw->GetSizer()->Detach(pw);
#ifndef __WXMAC__
                // this causes segfault on OSX
                pw->Destroy();
#endif
                sw->Layout();
                //std::printf("count: %zu\n", sw->GetSizer()->GetItemCount());
            }
            break;
        }
        case DT_STASHITEM: {
            /* We're dropping a StashItem right back in the stash. */
            StashItem *si = (StashItem *)thing_being_dragged;
            assert(0 <= si->n && si->n <= 2);
            si->n += 1;
            si->Refresh();
            break;
        }
        default: assert(false);
    }
    thing_being_dragged = NULL;
}

