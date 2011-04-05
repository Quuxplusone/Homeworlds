for f in *.raw; do echo $f; perl -w ./cook.pl < $f > ${f/.raw/}.cooked; done

