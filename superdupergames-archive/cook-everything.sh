for f in *.raw; do perl -w ./cook.pl < $f > ${f/.raw/}.cooked; done

