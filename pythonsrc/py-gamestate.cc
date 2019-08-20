#include <Python.h>
#include <string>
#include "core-src/state.h"
#include "core-src/AI.h"
#include "core-src/ApplyMove.h"
#include "core-src/InferMove.h"
#include "core-src/WholeMove.h"
#include "pythonsrc/glue.h"

namespace {

void turn_applymove_result_into_python_exception(ApplyMove::Result r)
{
    switch (r) {
        case ApplyMove::Result::SUICIDE:
            PyErr_SetString(PyExc_ValueError, "The move as parsed was disallowed by the rule against self-destruction.");
            break;
        case ApplyMove::Result::UNKNOWN_NAME:
            PyErr_SetString(PyExc_ValueError, "The move as parsed referred to a nonexistent star system.");
            break;
        case ApplyMove::Result::DUPLICATE_NAME:
            PyErr_SetString(PyExc_ValueError, "The move as parsed tried to create a new star system with the same name as an existing one.");
            break;
        case ApplyMove::Result::AMBIGUOUS:
            PyErr_SetString(PyExc_ValueError, "The move as parsed was incomplete or ambiguous.");
            break;
        case ApplyMove::Result::IMPOSSIBLE:
            PyErr_SetString(PyExc_ValueError, "The move as parsed was disallowed by the rules.");
            break;
        case ApplyMove::Result::NOT_DURING_SETUP:
            PyErr_SetString(PyExc_ValueError, "The move as parsed is not permitted during the setup phase.");
            break;
        case ApplyMove::Result::ONLY_DURING_SETUP:
            PyErr_SetString(PyExc_ValueError, "The move as parsed is not permitted outside of the setup phase.");
            break;
        default:
            PyErr_SetString(PyExc_RuntimeError, "unexpected ApplyMove::Result value");
            break;
    }
}

struct GameStatePyObject {
    PyObject_HEAD
    GameState *obj_;

    static int init(PyObject *self_, PyObject *args, PyObject *kwargs) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            const char *text = nullptr;
            if (!PyArg_ParseTuple(args, "s", &text)) {
                return -1;
            }
            self->obj_ = new GameState;
            if (self->obj_->scan(text) == false) {
                PyErr_SetString(PyExc_ValueError, "text did not parse as a game state");
                return -1;
            }
        } catch (...) {
            turn_current_exception_into_python_exception();
            return -1;
        }

        return 0;
    }

    static void dealloc(PyObject *self_) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        delete self->obj_;
        Py_TYPE(self)->tp_free(self);
    }

    static PyObject *toString(PyObject *self_, PyObject *args) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            if (!PyArg_ParseTuple(args, "")) {
                return nullptr;
            }
            std::string result = self->obj_->toString();
            return Py_BuildValue("s", result.c_str());
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *repr(PyObject *self_) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            UniquePyObjectPtr text = Py_BuildValue("s", self->obj_->toString().c_str());
            UniquePyObjectPtr repr_of_text = PyObject_Repr(text.get());
            UniquePyObjectPtr bytes_in_repr_of_text = PyUnicode_AsUTF8String(repr_of_text.get());
            const char *s = PyBytes_AsString(bytes_in_repr_of_text.get());
            std::string result = "libannotate.GameState(" + std::string(s) + ")";
            return Py_BuildValue("s", result.c_str());
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *str(PyObject *self_) {
        try {
            GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
            std::string result = self->obj_->toString();
            return Py_BuildValue("s", result.c_str());
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *deepcopy(PyObject *self_, PyObject *memo) {
        try {
            GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
            return wrap_GameState_instance(*self->obj_);
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *getBestMove(PyObject *self_, PyObject *args) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            int attacker = 0;
            if (!PyArg_ParseTuple(args, "i", &attacker)) {
                return nullptr;
            }
            if (!(attacker == 0 || attacker == 1)) {
                PyErr_SetString(PyExc_ValueError, "attacker must be 0 or 1");
                return nullptr;
            }
            if (self->obj_->gameIsOver() && !self->obj_->mightBeSettingUpHomeworldFor(attacker)) {
                PyErr_SetString(PyExc_ValueError, "the game appears to be over");
                return nullptr;
            }
            WholeMove m = get_ai_move(*self->obj_, attacker);
            return wrap_WholeMove_instance(std::move(m));
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *gameIsOver(PyObject *self_, PyObject *args) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            if (!PyArg_ParseTuple(args, "")) {
                return nullptr;
            }
            return Py_BuildValue("b", int(self->obj_->gameIsOver()));
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *apply(PyObject *self_, PyObject *args) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            UniquePyObjectPtr t = make_WholeMovePyObject_type();
            int attacker = 0;
            PyObject *move_ = nullptr;
            if (!PyArg_ParseTuple(args, "iO!", &attacker, t.get(), &move_)) {
                return nullptr;
            }
            if (!(attacker == 0 || attacker == 1)) {
                PyErr_SetString(PyExc_ValueError, "attacker must be 0 or 1");
                return nullptr;
            }
            GameState& st = *self->obj_;
            WholeMove move = *unwrap_WholeMove_instance(move_);
            if (move.isMissingPieces()) {
                WholeMove newmove = move;
                if (inferMoveFromState(st, attacker, &newmove)) {
                    move = std::move(newmove);
                }
            }
            ApplyMove::Result r = ApplyMove::Whole(st, attacker, move);
            if (r != ApplyMove::SUCCESS) {
                turn_applymove_result_into_python_exception(r);
                return nullptr;
            }
            return Py_BuildValue("");
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *copyApply(PyObject *self_, PyObject *args) {
        GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_);
        try {
            UniquePyObjectPtr t = make_WholeMovePyObject_type();
            int attacker = 0;
            PyObject *move_ = nullptr;
            if (!PyArg_ParseTuple(args, "iO!", &attacker, t.get(), &move_)) {
                return nullptr;
            }
            if (!(attacker == 0 || attacker == 1)) {
                PyErr_SetString(PyExc_ValueError, "attacker must be 0 or 1");
                return nullptr;
            }
            GameState st = *self->obj_;
            WholeMove move = *unwrap_WholeMove_instance(move_);
            if (move.isMissingPieces()) {
                WholeMove newmove = move;
                if (inferMoveFromState(st, attacker, &newmove)) {
                    move = std::move(newmove);
                }
            }
            ApplyMove::Result r = ApplyMove::Whole(st, attacker, move);
            if (r != ApplyMove::SUCCESS) {
                turn_applymove_result_into_python_exception(r);
                return nullptr;
            }
            return wrap_GameState_instance(std::move(st));
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }
};

} // anonymous namespace

PyObject *wrap_GameState_instance(GameState st) {
    UniquePyObjectPtr t_ = make_GameStatePyObject_type();
    PyTypeObject *t = reinterpret_cast<PyTypeObject *>(t_.get());
    UniquePyObjectPtr self_ = t->tp_alloc(t, 0);
    GameStatePyObject *self = reinterpret_cast<GameStatePyObject *>(self_.get());
    self->obj_ = new GameState(std::move(st));
    return self_.release();
}

PyObject *make_GameStatePyObject_type()
{
    static constexpr PyMethodDef methods[] = {
        { "toString", GameStatePyObject::toString, METH_VARARGS, "convert to string" },
        { "__deepcopy__", GameStatePyObject::deepcopy, METH_VARARGS, "make a deep copy" },
        { "getBestMove", GameStatePyObject::getBestMove, METH_VARARGS, "get the best move for the given player" },
        { "copyApply", GameStatePyObject::copyApply, METH_VARARGS, "return the new GameState after applying a move" },
        { "apply", GameStatePyObject::apply, METH_VARARGS, "modify this GameState in place by applying a move" },
        { "gameIsOver", GameStatePyObject::gameIsOver, METH_VARARGS, "return True if the game is over" },
        { nullptr, nullptr, 0, nullptr },
    };

    static PyTypeObject type = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "libannotate.GameState",  // tp_name
        sizeof(GameStatePyObject),  // tp_basicsize
        0,        // tp_itemsize
        GameStatePyObject::dealloc,  // tp_dealloc
        nullptr,  // tp_print
        nullptr,  // tp_getattr
        nullptr,  // tp_setattr
        nullptr,  // tp_as_async
        GameStatePyObject::repr,  // tp_repr
        nullptr,  // tp_as_number
        nullptr,  // tp_as_sequence
        nullptr,  // tp_as_mapping
        nullptr,  // tp_hash
        nullptr,  // tp_call
        GameStatePyObject::str,  // tp_str
        nullptr,  // tp_getattro
        nullptr,  // tp_setattro
        nullptr,  // tp_as_buffer
        Py_TPFLAGS_DEFAULT,  // tp_flags
        "A game state.",  // tp_doc
        nullptr,  // tp_traverse
        nullptr,  // tp_clear
        nullptr,  // tp_richcompare
        0,        // tp_weaklistoffset
        nullptr,  // tp_iter
        nullptr,  // tp_iternext
        const_cast<PyMethodDef*>(methods),  // tp_methods
        nullptr,  // tp_members
        nullptr,  // tp_getset
        nullptr,  // tp_base
        nullptr,  // tp_dict
        nullptr,  // tp_descr_get
        nullptr,  // tp_descr_set
        0,        // tp_dictoffset
        GameStatePyObject::init,  // tp_init
        nullptr,  // tp_alloc
        PyType_GenericNew,  // tp_new
        nullptr,  // tp_free
        nullptr,  // tp_is_gc
        nullptr,  // tp_bases
        nullptr,  // tp_mro
        nullptr,  // tp_cache
        nullptr,  // tp_subclasses
        nullptr,  // tp_weaklist
        nullptr,  // tp_del
        0,        // tp_version_tag
        nullptr,  // tp_finalize
    };

    if (PyType_Ready(&type) != 0) {
        return nullptr;
    }
    Py_INCREF(&type);
    return reinterpret_cast<PyObject *>(&type);
}
