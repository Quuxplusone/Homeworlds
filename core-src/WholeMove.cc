
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>
#include "SingleAction.h"
#include "WholeMove.h"
#include "mprintf.h"
#include "state.h"

template<class T>
class FreeOnReturn {
    T &ptr;
public:
    FreeOnReturn(T &p): ptr(p) { }
    ~FreeOnReturn() { free(ptr); }
};

/* A WholeMove is valid only if it consists of a possibly empty
 * sequence of catastrophes, followed by an optional single sacrifice,
 * followed by an appropriate number of regular actions, and finished
 * with another possibly empty sequence of catastrophes. */
bool WholeMove::sanitycheck() const
{
    int i = 0;
    int n = actions.size();
    while (i < n && actions[i].kind == CATASTROPHE) {
        if (!actions[i].sanitycheck()) return false;
        ++i;
    }
    if (i == n) {
        return true;
    }

    if (actions[i].kind == SACRIFICE) {
        if (!actions[i].sanitycheck()) return false;
        Color sac_color = actions[i].piece.color;
        Size sac_size = actions[i].piece.size;
        int entitled_actions = (sac_size == UNKNOWN_SIZE) ? 3 : (1+(int)sac_size);
        int last_entitled_action_idx = i + entitled_actions;
        ++i;
        /* All the actions from here up to the final sequence
         * of catastrophes must be of the proper color. */
        while (i < n && actions[i].kind != CATASTROPHE) {
            if (!actions[i].sanitycheck()) return false;
            switch (actions[i].kind) {
                case HOMEWORLD: return false;
                case CATASTROPHE: assert(false); return false;
                case SACRIFICE: return false;
                case CAPTURE: if (sac_color != UNKNOWN_COLOR && sac_color != RED) return false; break;
                case MOVE: if (sac_color != UNKNOWN_COLOR && sac_color != YELLOW) return false; break;
                case MOVE_CREATE: if (sac_color != UNKNOWN_COLOR && sac_color != YELLOW) return false; break;
                case BUILD: if (sac_color != UNKNOWN_COLOR && sac_color != GREEN) return false; break;
                case CONVERT: if (sac_color != UNKNOWN_COLOR && sac_color != BLUE) return false; break;
                default: assert(false); return false;
            }
            ++i;
        }
        /* You can't take more actions than you sacrificed for. */
        if (i > last_entitled_action_idx+1) return false;
    } else {
        /* A single free action. */
        if (!actions[i].sanitycheck()) return false;
        if (actions[i].kind == HOMEWORLD && n != 1) return false;
        ++i;
    }
    /* The move may end with a sequence of catastrophes. */
    while (i < n && actions[i].kind == CATASTROPHE) {
        ++i;
    }
    return (i == n);
}

bool WholeMove::isMissingPieces() const
{
    for (const auto& action : actions) {
        if (action.isMissingPieces()) return true;
    }
    return false;
}

bool WholeMove::isMissingPiecesNeededForSDGString() const
{
    for (const auto& action : actions) {
        if (action.isMissingPiecesNeededForSDGString()) return true;
    }
    return false;
}

int WholeMove::unusedSacrificeActions() const
{
    int entitled_actions = 0;
    int used_actions = 0;
    for (int i=0; i < (int)actions.size(); ++i) {
        switch (actions[i].kind) {
            case HOMEWORLD: break;
            case CATASTROPHE: break;
            case SACRIFICE: {
                Size sac_size = actions[i].piece.size;
                entitled_actions = (sac_size == UNKNOWN_SIZE) ? 3 : (1+(int)sac_size);
                break;
            }
            case CAPTURE: case MOVE: case MOVE_CREATE:
            case BUILD: case CONVERT: {
                used_actions += 1;
                break;
            }
            default: assert(false); break;
        }
    }
    return (entitled_actions == 0) ? 0 : (entitled_actions - used_actions);
}

bool WholeMove::scan(const char *text)
{
    actions.clear();
    if (*text == '\0') {
        /* Safety catch: An empty string does not mean "pass". */
        return false;
    } else if (!strcmp(text, "pass")) {
        assert(this->isPass());
        return true;
    }
    char *buffer = nullptr;
    FreeOnReturn<char *> fr(buffer);
    const char *semicolon;
    while (*text != '\0') {
        bool success;
        semicolon = strchr(text, ';');
        if (semicolon == nullptr) {
            success = SingleAction::scan_for_multibuild(text, actions)
                   || SingleAction::scan_for_multicapture(text, actions)
                   || SingleAction::scan_for_multimove(text, actions);
            if (!success) {
                actions.push_back(SingleAction());
                success = actions.back().scan(text);
                if (!success) return false;
            }
            break;
        } else {
            if (semicolon[1] != ' ') return false;
            buffer = (char *)realloc(buffer, semicolon - text + 1);
            assert(buffer != nullptr);
            memcpy(buffer, text, semicolon - text);
            buffer[semicolon - text] = '\0';
            success = SingleAction::scan_for_multibuild(buffer, actions)
                   || SingleAction::scan_for_multicapture(buffer, actions)
                   || SingleAction::scan_for_multimove(buffer, actions);
            if (!success) {
                actions.push_back(SingleAction());
                success = actions.back().scan(buffer);
                if (!success) return false;
            }
            text = semicolon+2;
        }
    }
    return sanitycheck();
}


std::string WholeMove::toString() const
{
    assert(this->sanitycheck());
    if (this->isPass()) {
        return "pass";
    } else {
        std::string result = "";
        for (int i=0; i < (int)actions.size(); ++i) {
            if (i > 0) result += "; ";
            result += actions[i].toString();
        }
        return result;
    }
}

std::string WholeMove::toSDGString() const
{
    assert(this->sanitycheck());
    if (this->isPass()) {
        return "pass";
    } else {
        std::string result = "";
        for (int i=0; i < (int)actions.size(); ++i) {
            if (i > 0) result += "; ";
            result += actions[i].toSDGString();
        }
        for (int i=0; i < this->unusedSacrificeActions(); ++i) {
            result += "; pass";
        }
        return result;
    }
}
