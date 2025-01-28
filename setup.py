#!/usr/bin/env python

from setuptools import setup
from distutils.core import Extension

setup(
    name='libhomeworlds',
    version='1.1.0',
    install_requires=[
        # pip requirements go here; at the moment there are none
    ],
    package_data={'': ['LICENSE']},

    ext_modules=[
        Extension(
            'libhomeworlds',
            sources=[
                'pythonsrc/py-libhomeworlds.cc',
                'pythonsrc/py-wholemove.cc',
                'pythonsrc/py-gamestate.cc',
                'core-src/AIMove.cc',
                'core-src/AIStaticEval.cc',
                'core-src/AllMoves.cc',
                'core-src/ApplyMove.cc',
                'core-src/GameState.cc',
                'core-src/InferMove.cc',
                'core-src/PieceCollection.cc',
                'core-src/PlanetNames.cc',
                'core-src/SingleAction.cc',
                'core-src/StarSystem.cc',
                'core-src/WholeMove.cc',
                'core-src/mprintf.cc',
            ],
            extra_compile_args=[
                '-std=c++14',
            ],
        ),
    ],
    include_dirs=[
        '.',
    ],
    test_suite='pythonsrc.tests',

    # metadata for upload to PyPI
    author="Arthur O'Dwyer",
    author_email='arthur.j.odwyer@gmail.com',
    description="AI for Binary Homeworlds, written in C++.",
    license='http://creativecommons.org/licenses/BSD/',
    long_description=None,
    keywords='ai bot homeworlds game labs looney superdupergames',
    url='https://github.com/Quuxplusone/Homeworlds',
)
