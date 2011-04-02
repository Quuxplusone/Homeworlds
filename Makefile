
CC = gcc
CXX = g++
# The release setup.
#CFLAGS = -O99 -march=native -ansi -pedantic -W -Wall -Wextra -fomit-frame-pointer -DNDEBUG
# The profiling setup.
#CFLAGS = -O99 -march=native -ansi -pedantic -W -Wall -Wextra -pg -DNDEBUG
# The debug setup.
CFLAGS = -O2 -ansi -pedantic -W -Wall -Wextra
# Replace this line to use a different evaluation function.
AIEVAL = AIStaticEval3.cc

OBJS = mprintf.o PieceCollection.o StarSystem.o GameState.o WholeMove.o ApplyMove.o
HFILES = global.h state.h move.h ApplyMove.h AllMoves.h PlanetNames.h getline.h AI.h AlphaBeta.h

all: annotate

annotate: annotatemain.o getline.o AllMoves.o InferMove.o PlanetNames.o AIMove.o AIStaticEval.o ${OBJS}
	${CXX} ${CFLAGS} $^ -o $@

AIStaticEval.o: ${AIEVAL}
	${CXX} ${CFLAGS} $^ -c -o $@

getline.o: getline.c
	${CC} ${CFLAGS} $^ -c -o $@

%.o: %.cc ${HFILES}
	${CXX} ${CFLAGS} $< -c -o $@

clean:
	rm *.o annotate
