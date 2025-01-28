#include <Python.h>
#include <string>
#include "core-src/WholeMove.h"
#include "pythonsrc/glue.h"

namespace {

struct WholeMovePyObject {
    PyObject_HEAD
    WholeMove *obj_;

    static int init(PyObject *self_, PyObject *args, PyObject *kwargs) {
        WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
        const char *text = nullptr;
        if (!PyArg_ParseTuple(args, "s", &text)) {
            // `PyArg_ParseTuple` has already taken care of setting up the
            // appropriate exception to be raised in this case.
            return -1;
        }
        try {
            self->obj_ = new WholeMove;
            if (self->obj_->scan(text) == false) {
                PyErr_SetString(PyExc_ValueError, "text did not parse as a move");
                return -1;
            }
        } catch (...) {
            turn_current_exception_into_python_exception();
            return -1;
        }

        return 0;
    }

    static void dealloc(PyObject *self_) {
        WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
        delete self->obj_;
        Py_TYPE(self)->tp_free(self);
    }

    static PyObject *toString(PyObject *self_, PyObject *args) {
        WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
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

    static PyObject *toSDGString(PyObject *self_, PyObject *args) {
        WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
        try {
            if (!PyArg_ParseTuple(args, "")) {
                return nullptr;
            }
            if (self->obj_->isMissingPieces()) {
                PyErr_SetString(PyExc_ValueError, "move with missing information cannot be stringified into SDG format");
                return nullptr;
            }
            std::string result = self->obj_->toSDGString();
            return Py_BuildValue("s", result.c_str());
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *repr(PyObject *self_) {
        WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
        try {
            UniquePyObjectPtr text = Py_BuildValue("s", self->obj_->toString().c_str());
            UniquePyObjectPtr repr_of_text = PyObject_Repr(text.get());
            UniquePyObjectPtr bytes_in_repr_of_text = PyUnicode_AsUTF8String(repr_of_text.get());
            const char *s = PyBytes_AsString(bytes_in_repr_of_text.get());
            std::string result = "libhomeworlds.WholeMove(" + std::string(s) + ")";
            return Py_BuildValue("s", result.c_str());
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *str(PyObject *self_) {
        try {
            WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
            std::string result = self->obj_->toString();
            return Py_BuildValue("s", result.c_str());
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }

    static PyObject *deepcopy(PyObject *self_, PyObject *memo) {
        try {
            WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
            return wrap_WholeMove_instance(*self->obj_);
        } catch (...) {
            turn_current_exception_into_python_exception();
            return nullptr;
        }
    }
};

} // anonymous namespace

PyObject *wrap_WholeMove_instance(WholeMove m) {
    UniquePyObjectPtr t_ = make_WholeMovePyObject_type();
    PyTypeObject *t = reinterpret_cast<PyTypeObject*>(t_.get());
    UniquePyObjectPtr self_ = t->tp_alloc(t, 0);
    WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_.get());
    self->obj_ = new WholeMove(std::move(m));
    return self_.release();
}

WholeMove *unwrap_WholeMove_instance(PyObject *self_) {
    WholeMovePyObject *self = reinterpret_cast<WholeMovePyObject *>(self_);
    return self->obj_;
}

PyObject *make_WholeMovePyObject_type()
{
    static constexpr PyMethodDef methods[] = {
        { "toString", WholeMovePyObject::toString, METH_VARARGS, "convert to string" },
        { "toSDGString", WholeMovePyObject::toSDGString, METH_VARARGS, "convert to string in SuperDuperGames format" },
        { nullptr, nullptr, 0, nullptr },
    };

    static PyTypeObject type = {
        PyVarObject_HEAD_INIT(NULL, 0)
        "libhomeworlds.WholeMove",  // tp_name
        sizeof(WholeMovePyObject),  // tp_basicsize
        0,        // tp_itemsize
        WholeMovePyObject::dealloc,  // tp_dealloc
        0,        // tp_vectorcall_offset
        nullptr,  // tp_getattr
        nullptr,  // tp_setattr
        nullptr,  // tp_as_async
        WholeMovePyObject::repr,  // tp_repr
        nullptr,  // tp_as_number
        nullptr,  // tp_as_sequence
        nullptr,  // tp_as_mapping
        nullptr,  // tp_hash
        nullptr,  // tp_call
        WholeMovePyObject::str,  // tp_str
        nullptr,  // tp_getattro
        nullptr,  // tp_setattro
        nullptr,  // tp_as_buffer
        Py_TPFLAGS_DEFAULT,  // tp_flags
        "A whole move, possibly consisting of several actions.",  // tp_doc
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
        WholeMovePyObject::init,  // tp_init
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
        // `PyType_Ready` has already taken care of setting up the
        // appropriate exception to be raised in this case.
        return nullptr;
    }
    Py_INCREF(&type);
    return reinterpret_cast<PyObject *>(&type);
}
