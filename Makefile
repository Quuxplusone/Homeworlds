UNAME := $(shell uname -s)

INCLUDES = -Icore-src
CFLAGS += -W -Wall -Wextra -march=native ${INCLUDES}
CXXFLAGS += -std=c++14

# The release setup.
#CFLAGS += -O3 -fomit-frame-pointer -DNDEBUG
# The profiling setup.
#CFLAGS += -O3 -pg -DNDEBUG
# The debug setup.
CFLAGS += -O2 -g

# Replace this line to use a different evaluation function.
AIEVAL = core-src/AIStaticEval3.cc

OBJS = mprintf.o PieceCollection.o StarSystem.o GameState.o SingleAction.o WholeMove.o ApplyMove.o
AIOBJS = AllMoves.o AIMove.o AIStaticEval.o PlanetNames.o ${OBJS}

all: annotate wxgui

test: test-core
	./test-core

annotate: annotatemain.o getline.o InferMove.o ${AIOBJS}
	${CXX} ${CFLAGS} ${CXXFLAGS} $^ -o $@

test-core: PieceCollection.t.o StarSystem.t.o GameState.t.o WholeMove.t.o AllMoves.t.o ${OBJS} AllMoves.o InferMove.o ApplyMove.o
	${CXX} ${CFLAGS} ${CXXFLAGS} $^ -lgtest -lgtest_main -o $@

wxgui: wxmain.o wxPiece.o wxSystem.o wxStash.o wxGalaxy.o wxMouse.o getline.o InferMove.o ${AIOBJS}
	${CXX} ${CFLAGS} ${CXXFLAGS} $^ `wx-config --libs` -o $@

AIStaticEval.o: ${AIEVAL} core-src/*.h
	${CXX} ${CFLAGS} ${CXXFLAGS} $< -c -o $@

getline.o: core-src/getline.c core-src/getline.h
	${CC} ${CFLAGS} $< -c -o $@

%.o: core-src/%.cc core-src/*.h
	${CXX} ${CFLAGS} ${CXXFLAGS} $< -c -o $@

%.o: wxgui-src/%.cc wxgui-src/*.h core-src/*.h
	${CXX} ${CFLAGS} ${CXXFLAGS} $< `wx-config --cppflags` -Wno-potentially-evaluated-expression -c -o $@

clean:
	rm -f *.o annotate wxgui test-core

.PHONY: all clean test
