time for j in `seq 40`; do for i in `seq 40`; do echo ai_move; done | ./annotate --seed $j Sam Dave >/dev/null; done
time for j in `seq 48`; do ./annotate --auto < benchmarks/perf-27635-moves.txt > /dev/null; done
time for j in `seq 30`; do ./annotate --auto < benchmarks/perf-33332-moves.txt > /dev/null; done

# make clean; CXXFLAGS='-std=c++03 -O3 -DNDEBUG' make -j8 annotate; ./run-benchmarks.sh

# make clean; CXXFLAGS='-std=c++11 -O3 -DNDEBUG' make -j8 annotate; ./run-benchmarks.sh

# make clean; CXXFLAGS='-std=c++1y -O3 -DNDEBUG' make -j8 annotate; ./run-benchmarks.sh
