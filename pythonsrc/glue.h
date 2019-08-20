#pragma once

#include <Python.h>
#include <exception>
#include <utility>
#include "core-src/WholeMove.h"
#include "core-src/state.h"

class UniquePyObjectPtr {
    PyObject *p_ = nullptr;
public:
    /*implicit*/ UniquePyObjectPtr(PyObject *p) : p_(p) {}
    explicit operator bool() const noexcept { return p_ != nullptr; }
    PyObject *get() const noexcept { return p_; }
    PyObject *release() noexcept { return std::exchange(p_, nullptr); }
    ~UniquePyObjectPtr() {
        if (p_) {
            Py_DECREF(p_);
        }
    }
};

inline void turn_current_exception_into_python_exception()
{
    try {
        throw;
    } catch (const std::exception& ex) {
        PyErr_SetString(PyExc_RuntimeError, ex.what());
    } catch (...) {
        PyErr_SetString(PyExc_RuntimeError, "unknown C++ exception");
    }
}

PyObject *make_WholeMovePyObject_type();
PyObject *make_GameStatePyObject_type();

PyObject *wrap_WholeMove_instance(WholeMove m);
PyObject *wrap_GameState_instance(GameState st);

WholeMove *unwrap_WholeMove_instance(PyObject *);
