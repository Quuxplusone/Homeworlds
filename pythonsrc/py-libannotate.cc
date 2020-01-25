#include <Python.h>
#include "pythonsrc/glue.h"

static PyObject *
newGame(PyObject *self, PyObject *args)
{
    GameState st;
    st.newGame();
    return wrap_GameState_instance(std::move(st));
}

PyMODINIT_FUNC PyInit_libannotate()
{
    // https://docs.python.org/3/extending/extending.html#the-module-s-method-table-and-initialization-function
    static constexpr PyMethodDef methods[] = {
        {"newGame", newGame, METH_NOARGS, "Return a GameState object representing a brand-new game."},
        {nullptr, nullptr, 0, nullptr},
    };

    // https://docs.python.org/3/c-api/module.html
    static PyModuleDef module = {
        PyModuleDef_HEAD_INIT,
        "libannotate",  // m_name
        "AI for Binary Homeworlds, written in C++.",  // m_doc
        0,        // m_size
        const_cast<PyMethodDef*>(methods),  // m_methods
        nullptr,  // m_slots
        nullptr,  // m_traverse
        nullptr,  // m_clear
        nullptr,  // m_free
    };

    if (UniquePyObjectPtr m = PyModule_Create(&module)) {
        if (PyObject *t = make_WholeMovePyObject_type()) {
            PyModule_AddObject(m.get(), "WholeMove", t);
        } else {
            return nullptr;
        }
        if (PyObject *t = make_GameStatePyObject_type()) {
            PyModule_AddObject(m.get(), "GameState", t);
        } else {
            return nullptr;
        }
        return m.release();
    } else {
        return nullptr;
    }
}
