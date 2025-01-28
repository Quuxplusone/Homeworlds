time for j in `seq 40`; do for i in `seq 40`; do echo ai_move; done | ./homeworlds-cli --seed $j Sam Dave >/dev/null; done
time for j in `seq 48`; do ./homeworlds-cli --auto < benchmarks/perf-27635-moves.txt > /dev/null; done
time for j in `seq 30`; do ./homeworlds-cli --auto < benchmarks/perf-33332-moves.txt > /dev/null; done

# make clean; CXXFLAGS='-std=c++03 -O3 -DNDEBUG' make -j8 homeworlds-cli; ./run-benchmarks.sh

# make clean; CXXFLAGS='-std=c++11 -O3 -DNDEBUG' make -j8 homeworlds-cli; ./run-benchmarks.sh

# make clean; CXXFLAGS='-std=c++14 -O3 -DNDEBUG' make -j8 homeworlds-cli; ./run-benchmarks.sh

# make clean; CXXFLAGS='-std=c++17 -O3 -DNDEBUG' make -j8 homeworlds-cli; ./run-benchmarks.sh
