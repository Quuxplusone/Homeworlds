# Homeworlds

This is a C++ framework for working with the game of Binary Homeworlds,
a two-player game of strategy and tactics that might aptly be described
as "space chess".  Homeworlds is normally played with a set of Icehouse
pieces in four colors, and without a board; the pieces shift on and off
the table during the game to represent star systems and spaceships.
The goal of the game is to eliminate all of the ships defending your
enemy's homeworld: by moving in a larger ship to capture them, by
sending in fleets of the appropriate colors to catastrophe them, or by
sending in those fleets to catastrophe the enemy's binary star system
itself.


## Components

This project consists of five main components:


### C++14 library code in `core-src/`

This poorly structured library includes C++ data structures for
`SingleAction`, `WholeMove`, `PieceCollection`, `StarSystem`, and `GameState`.
It also includes these facilities:

- `ApplyMove`: apply a `WholeMove` to a `GameState`

- `AllMoves`: generate all possible `WholeMove`s from a given `GameState`

- `InferMove`: "fill in the blanks" in an incomplete `WholeMove` such as `build g1`,
    given the current `GameState`

- `AIMove`: pick the best move from among all the possible `WholeMove`s following
    the current `GameState`


### `annotate`, a command-line game analyzer

C++14 source code in `core-src/`.

This command-line utility brings together all of the above library code.
You can use it to play a game against the computer, by alternately entering
your own moves and the special command `ai_move`.

To run the C++ unit tests, first install GTest
(instructions are [here](https://stackoverflow.com/a/46611467/1424877))
and then run `make test`.


### `wxgui`, a graphical game interface

C++14 source code in `wxgui-src/`.

This GUI program uses the wxWidgets library, and is my experiment with
a graphical Homeworlds program. Use the mouse to drag pieces around on the
board, and then click "Done" to end your turn. The program will take care
of figuring out what move you made to get from A to B.


### `libannotate`, a Python extension

Python and C++14 source code in `setup.py` and `pythonsrc/`.

This Python extension wraps up the C++ library code into a much simpler and cleaner
Python module with just a few high-level entry points. Python class types are
provided only for `GameState` and `WholeMove`.

    import libannotate
    st = libannotate.GameState('Foo (0,b3r1) g1g3-\nBar(1,b3y2) -g3')
    m = st.getBestMove(0)
    print (m.toString())

For more examples of usage, see `pythonsrc/tests.py`.


### SuperDuperGames.org archive

Python source code and text files in `superdupergames-archive/`.

A partial mirror of the [SuperDuperGames.org](http://superdupergames.org)
game archive, containing over 700 game transcripts to date.
Includes a Python script to translate the archive games from SDG's format
into the format expected by `annotate` and `libannotate`.


## Patches wanted

Do you know how to make the wxWidgets GUI look better? Please submit
a patch! If you don't want to deal with the GitHub overhead, you can
send me an email.

Do you think you can make a better AI? Again, please submit a patch!
I know the current AI is very weak. The major hurdle is that it's not always
feasible to look more than one move ahead, unless you can figure out
how to prune bad moves. Homeworlds has a much higher branching factor
than either chess or go. Another hurdle is that my internal representations
of game states and actions are fairly verbose and hard to manipulate
(in terms of CPU cycles).


## External links

- http://boardgamegeek.com/boardgame/14634/homeworlds

- http://www.wunderland.com/WTS/Andy/Games/ILoveHomeworlds.html

- http://superdupergames.org/main.html

This project is not affiliated with SuperDuperGames.org, BoardGameGeek.com, or Looney Labs.
