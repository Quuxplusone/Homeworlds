
#ifndef H_WXSTUFF
 #define H_WXSTUFF

#include <string>
#include <wx/wx.h>
#include <wx/panel.h>
#include "state.h"

struct SystemWidget;

/* PieceWidget is the widget which represents a single piece.  Each piece
 * has a given size, color, and orientation.  It is the child of a certain
 * panel (member "parent"), and can be drag-and-dropped to certain other
 * panels. */
struct PieceWidget : public wxPanel
{
    int whose;
    Size piece_size;
    Color piece_color;
    /* if orientation == UPRIGHT */
    Size second_size;
    Color second_color;

    PieceWidget(SystemWidget *parent, int id, int whose, Size s, Color c);
    PieceWidget(SystemWidget *parent, int id, Size s, Color c, Size s2, Color c2);
    void mouseup();

    virtual void OnSize(wxSizeEvent &event);
    virtual void OnPaint(wxPaintEvent &event);
};

struct SystemWidget : public wxPanel
{
    std::string name;  /* name of the system in our GameState */
    PieceWidget *star;

    SystemWidget(wxWindow *parent, int id, wxPoint pos);
    void update(const StarSystem *star);
    void add_star(PieceWidget *pw);
    void add_star(Size s, Color c);
    void add_star(Size s1, Color c1, Size s2, Color c2);
    void add_ship(int who, Size s, Color c);
    void add_ship(PieceWidget *pw);
};

struct StashWidget : public wxWindow
{
    StashWidget(wxWindow *parent, int id);
    void update(const PieceCollection &pc);
    void add_piece(Size s, Color c);
    void mouseup();
};

struct StashItem : public wxPanel
{
    const Color piece_color;
    const Size piece_size;
    int n;
    
    StashItem(StashWidget *p, Color c, Size s, int n_);
    void mousedown();

    virtual void OnSize(wxSizeEvent &event);
    virtual void OnPaint(wxPaintEvent &event);
};

struct GalaxyWidget : public wxWindow
{
    SystemWidget *attacker_homeworld;
    SystemWidget *defender_homeworld;
    StashWidget *stash;
    int num_systems;

    GalaxyWidget(wxWindow *parent, int id);
    SystemWidget *add_system(const wxPoint &pos, const std::string &name);
    void delete_system(SystemWidget *sw);
    void restash_and_delete_system(SystemWidget *sw);
    std::string get_new_system_name();
    SystemWidget *nth_system(int i);
    void update(const GameState &st);
    GameState to_state();
};

extern void setcolor(wxPaintDC &dc, Color c);


enum DraggableType { DT_PIECE, DT_STASHITEM };
extern DraggableType type_of_thing_being_dragged;
extern wxWindow *thing_being_dragged;

extern int global_attacker;
extern bool just_started_new_game;

enum {
    wxID_MAINWINDOW = 101,
    wxID_DEFENDER_HOMEWORLD_SYSTEM,
    wxID_ATTACKER_HOMEWORLD_SYSTEM,
    wxID_GALAXY_MAP,
    wxID_STASH_AREA,
    wxID_DONE_MOVE,
    wxID_CLEAR_MOVE,
    wxID_AI_MOVE,
};

#endif /* H_WXSTUFF */
